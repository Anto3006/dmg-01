mod memory;
mod registers;

use memory::Memory;
use registers::Registers;
use std::env;
use std::fs;

pub fn read_instructions() {
    let mut args = env::args();
    if args.len() > 1 {
        args.next();
        let file_name = args.next().unwrap();
        let bytes = fs::read(file_name).unwrap();
        for byte in bytes {
            let instruction = Instruction::from_byte(byte);
            match instruction {
                Some(instruction) => println!("{:?}", instruction),
                None => {
                    println!("Could not read instruction {byte:#x} {byte:#b}");
                    break;
                }
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum ArithmeticTarget8 {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Value(u8),
    HL,
    Address(ArithmeticTarget16),
}

#[derive(Clone, Copy, Debug)]
enum ArithmeticTarget16 {
    BC,
    DE,
    HL,
    SP,
    Value(u16),
}

#[derive(Clone, Copy, Debug)]
enum Instruction {
    NOP,
    ADD(ArithmeticTarget8),
    ADC(ArithmeticTarget8),
    ADD16(ArithmeticTarget16),
    ADDSP(i8),
    SUB(ArithmeticTarget8),
    SBC(ArithmeticTarget8),
    SUB16(ArithmeticTarget16),
    AND(ArithmeticTarget8),
    OR(ArithmeticTarget8),
    XOR(ArithmeticTarget8),
    COMPARE(ArithmeticTarget8),
    INC(ArithmeticTarget8),
    INC16(ArithmeticTarget16),
    DEC(ArithmeticTarget8),
    DEC16(ArithmeticTarget16),
    BIT(ArithmeticTarget8),
    CCF,
    CCA,
    JUMP(u16),
}

impl Instruction {
    fn from_byte(byte: u8) -> Option<Self> {
        match byte {
            0x00 => Some(Self::NOP),
            _ => None,
        }
    }
}

#[derive(Clone, Copy)]
enum JumpCondition {
    NotZero,
    Zero,
    NotCarry,
    Carry,
}

struct CPU {
    registers: Registers,
    memory: Memory,
    stack_pointer: u16,
    program_counter: u16,
}

impl CPU {
    fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::ADD(target) => self.add(target, false),
            Instruction::ADC(target) => self.add(target, true),
            Instruction::SUB(target) => self.sub(target, false),
            Instruction::SBC(target) => self.sub(target, true),
            Instruction::ADD16(target) => self.add_16(target),
            Instruction::ADDSP(value) => self.add_stack_pointer(value),
            _ => (),
        }
    }

    fn fetch_byte(&self) -> u8 {
        self.memory.get_memory_value(self.program_counter)
    }

    fn load(&mut self, target: ArithmeticTarget8, source: ArithmeticTarget8) {
        let value = self.get_target_value(source);
        self.change_target(target, value);
    }

    fn load_16(&mut self, target: ArithmeticTarget16, source: ArithmeticTarget16) {
        let value = self.get_target_value_16(source);
        self.change_target_16(target, value);
    }

    fn add(&mut self, target: ArithmeticTarget8, use_carry: bool) {
        let value = self.get_target_value(target);
        let (mut new_value, mut did_overflow) = self.registers.a.overflowing_add(value);
        if use_carry {
            let (new_value_carry, did_overflow_carry) =
                new_value.overflowing_add(self.registers.f.carry as u8);
            new_value = new_value_carry;
            did_overflow = did_overflow || did_overflow_carry;
        }
        self.registers.f.zero = new_value == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = self.did_half_carry_add(self.registers.a, value, use_carry);
        self.registers.f.carry = did_overflow;
        self.registers.a = new_value;
    }

    fn did_half_carry_add(&self, value1: u8, value2: u8, use_carry: bool) -> bool {
        if !use_carry {
            ((value1 & 0x0F) + (value2 & 0x0F)) > 0x0F
        } else {
            ((value1 & 0x0F) + (value2 & 0x0F)) + (self.registers.f.carry as u8) > 0x0F
        }
    }

    fn add_16(&mut self, target: ArithmeticTarget16) {
        let value = self.get_target_value_16(target);
        let (new_value, did_overflow) = self.registers.get_hl().overflowing_add(value);
        self.registers.f.substract = false;
        self.registers.f.half_carry =
            (self.registers.get_hl() & 0x0FFF) + (value & 0x0FFF) > 0x0FFF;
        self.registers.f.carry = did_overflow;
        self.registers.set_hl(new_value);
    }

    fn add_stack_pointer(&mut self, value: i8) {
        let new_value: u16;
        if value < 0 {
            new_value = self.stack_pointer - (-value as u16);
            self.registers.f.half_carry =
                (self.stack_pointer & 0x000F) - (-value as u16 & 0x000F) > 0x000F;
            self.registers.f.carry =
                (self.stack_pointer & 0x00FF) - (-value as u16 & 0x00FF) > 0x00FF;
        } else {
            new_value = self.stack_pointer + (value as u16);
            self.registers.f.half_carry =
                (self.stack_pointer & 0x000F) + (value as u16 & 0x000F) > 0x000F;
            self.registers.f.carry =
                (self.stack_pointer & 0x00FF) + (value as u16 & 0x00FF) > 0x00FF;
        }
        self.stack_pointer = new_value;
        self.registers.f.zero = false;
        self.registers.f.substract = false;
    }

    fn sub(&mut self, target: ArithmeticTarget8, use_carry: bool) {
        let value = self.get_target_value(target);
        let (mut new_value, mut did_borrow) = self.registers.a.overflowing_sub(value);
        if use_carry {
            let (new_value_carry, did_borrow_carry) =
                new_value.overflowing_sub(self.registers.f.carry as u8);
            new_value = new_value_carry;
            did_borrow = did_borrow || did_borrow_carry;
        }
        self.registers.f.zero = new_value == 0;
        self.registers.f.substract = true;
        self.registers.f.half_carry = self.did_half_carry_sub(self.registers.a, value, use_carry);
        self.registers.f.carry = did_borrow;
    }

    fn did_half_carry_sub(&self, value1: u8, value2: u8, use_carry: bool) -> bool {
        if !use_carry {
            (value1 & 0x0F) < (value2 & 0x0F)
        } else {
            (value1 & 0x0F) < ((value2 & 0x0F) + (self.registers.f.carry as u8))
        }
    }

    fn and(&mut self, target: ArithmeticTarget8) {
        let value = self.get_target_value(target);
        let result = self.registers.a & value;
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = true;
        self.registers.f.carry = false;
        self.registers.a = result;
    }

    fn or(&mut self, target: ArithmeticTarget8) {
        let value = self.get_target_value(target);
        let result = self.registers.a | value;
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
        self.registers.a = result;
    }

    fn xor(&mut self, target: ArithmeticTarget8) {
        let value = self.get_target_value(target);
        let result = self.registers.a ^ value;
        self.registers.f.zero = result == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = false;
        self.registers.a = result;
    }

    fn compare(&mut self, target: ArithmeticTarget8) {
        let value = self.get_target_value(target);
        let (new_value, did_borrow) = self.registers.a.overflowing_sub(value);
        self.registers.f.zero = new_value == 0;
        self.registers.f.substract = true;
        self.registers.f.half_carry = self.did_half_carry_sub(self.registers.a, value, false);
        self.registers.f.carry = did_borrow;
    }

    fn inc(&mut self, target: ArithmeticTarget8) {
        let value = self.get_target_value(target);
        let (new_value, _) = value.overflowing_add(1);
        self.change_target(target, new_value);
        self.registers.f.zero = new_value == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = self.did_half_carry_add(value, 1, false);
    }

    fn inc_16(&mut self, target: ArithmeticTarget16) {
        let value = self.get_target_value_16(target);
        let (new_value, _) = value.overflowing_add(1);
        self.change_target_16(target, new_value);
    }

    fn dec(&mut self, target: ArithmeticTarget8) {
        let value = self.get_target_value(target);
        let (new_value, _) = value.overflowing_sub(1);
        self.change_target(target, new_value);
        self.registers.f.zero = new_value == 0;
        self.registers.f.substract = true;
        self.registers.f.half_carry = self.did_half_carry_sub(value, 1, false);
    }

    fn dec_16(&mut self, target: ArithmeticTarget16) {
        let value = self.get_target_value_16(target);
        let (new_value, _) = value.overflowing_sub(1);
        self.change_target_16(target, new_value);
    }

    fn bit(&mut self, test_bit: u8, target: ArithmeticTarget8) {
        let value = self.get_target_value(target);
        let mask = 1 << test_bit;
        let bit_value = value & mask;
        self.registers.f.zero = bit_value == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = true;
    }

    fn complement_carry_flag(&mut self) {
        self.registers.f.substract = false;
        self.registers.f.half_carry = false;
        self.registers.f.carry = !self.registers.f.carry;
    }

    fn complment_accumulator(&mut self) {
        self.registers.a = !self.registers.a;
        self.registers.f.substract = true;
        self.registers.f.half_carry = true;
    }

    fn jump(&mut self, address: u16) {
        self.program_counter = address;
    }

    fn relative_jump(&mut self, offset: i8) {
        if offset > 0 {
            self.program_counter += offset as u16;
        } else {
            self.program_counter -= -offset as u16;
        }
    }

    fn jump_if_condition(&mut self, condition: JumpCondition, address: u16) {
        match condition {
            JumpCondition::NotZero => {
                if !self.registers.f.zero {
                    self.program_counter = address
                }
            }
            JumpCondition::Zero => {
                if self.registers.f.zero {
                    self.program_counter = address
                }
            }
            JumpCondition::NotCarry => {
                if !self.registers.f.carry {
                    self.program_counter = address
                }
            }
            JumpCondition::Carry => {
                if self.registers.f.carry {
                    self.program_counter = address
                }
            }
        }
    }

    fn relative_jump_if_condition(&mut self, condition: JumpCondition, offset: i8) {
        let new_address: u16;
        if offset > 0 {
            new_address = self.program_counter + (offset as u16);
        } else {
            new_address = self.program_counter - (-offset as u16);
        }
        match condition {
            JumpCondition::NotZero => {
                if !self.registers.f.zero {
                    self.program_counter = new_address
                }
            }
            JumpCondition::Zero => {
                if self.registers.f.zero {
                    self.program_counter = new_address
                }
            }
            JumpCondition::NotCarry => {
                if !self.registers.f.carry {
                    self.program_counter = new_address
                }
            }
            JumpCondition::Carry => {
                if self.registers.f.carry {
                    self.program_counter = new_address;
                }
            }
        }
    }

    fn change_target(&mut self, target: ArithmeticTarget8, new_value: u8) {
        match target {
            ArithmeticTarget8::A => self.registers.a = new_value,
            ArithmeticTarget8::B => self.registers.b = new_value,
            ArithmeticTarget8::C => self.registers.c = new_value,
            ArithmeticTarget8::D => self.registers.d = new_value,
            ArithmeticTarget8::E => self.registers.e = new_value,
            ArithmeticTarget8::H => self.registers.h = new_value,
            ArithmeticTarget8::L => self.registers.l = new_value,
            ArithmeticTarget8::HL => self
                .memory
                .set_memory_value(self.registers.get_hl(), new_value),
            ArithmeticTarget8::Value(_) => (),
            ArithmeticTarget8::Address(target) => {
                let address = self.get_target_value_16(target);
                self.memory.set_memory_value(address, new_value)
            }
        }
    }

    fn change_target_16(&mut self, target: ArithmeticTarget16, new_value: u16) {
        match target {
            ArithmeticTarget16::HL => self.registers.set_hl(new_value),
            ArithmeticTarget16::BC => self.registers.set_bc(new_value),
            ArithmeticTarget16::DE => self.registers.set_de(new_value),
            ArithmeticTarget16::SP => self.stack_pointer = new_value,
            ArithmeticTarget16::Value(_) => (),
        }
    }

    fn get_target_value(&self, target: ArithmeticTarget8) -> u8 {
        match target {
            ArithmeticTarget8::A => self.registers.a,
            ArithmeticTarget8::B => self.registers.b,
            ArithmeticTarget8::C => self.registers.c,
            ArithmeticTarget8::D => self.registers.d,
            ArithmeticTarget8::E => self.registers.e,
            ArithmeticTarget8::H => self.registers.h,
            ArithmeticTarget8::L => self.registers.l,
            ArithmeticTarget8::Value(value) => value,
            ArithmeticTarget8::HL => self.memory.get_memory_value(self.registers.get_hl()),
            ArithmeticTarget8::Address(target) => {
                let address = self.get_target_value_16(target);
                self.memory.get_memory_value(address)
            }
        }
    }

    fn get_target_value_16(&self, target: ArithmeticTarget16) -> u16 {
        match target {
            ArithmeticTarget16::HL => self.registers.get_hl(),
            ArithmeticTarget16::BC => self.registers.get_bc(),
            ArithmeticTarget16::DE => self.registers.get_de(),
            ArithmeticTarget16::SP => self.stack_pointer,
            ArithmeticTarget16::Value(value) => value,
        }
    }
}

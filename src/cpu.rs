mod registers;

use registers::Registers;

#[derive(Clone, Copy)]
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
}

#[derive(Clone, Copy)]
enum ArithmeticTarget16 {
    BC,
    DE,
    HL,
    SP,
    Value(u16),
}

#[derive(Clone, Copy)]
enum Instruction {
    ADD8(ArithmeticTarget8),
    ADC8(ArithmeticTarget8),
    SUB8(ArithmeticTarget8),
    SBC8(ArithmeticTarget8),
    AND8(ArithmeticTarget8),
}

struct CPU {
    registers: Registers,
}

impl CPU {
    fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::ADD8(target) => self.add(target, false),
            Instruction::ADC8(target) => self.add(target, true),
            Instruction::SUB8(target) => self.sub(target, false),
            Instruction::SBC8(target) => self.sub(target, true),
            _ => (),
        }
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
        self.change_register(target, new_value);
        self.registers.f.zero = new_value == 0;
        self.registers.f.substract = false;
        self.registers.f.half_carry = self.did_half_carry_add(value, 1, false);
    }

    fn dec(&mut self, target: ArithmeticTarget8) {
        let value = self.get_target_value(target);
        let (new_value, _) = value.overflowing_sub(1);
        self.change_register(target, new_value);
        self.registers.f.zero = new_value == 0;
        self.registers.f.substract = true;
        self.registers.f.half_carry = self.did_half_carry_sub(value, 1, false);
    }

    fn change_register(&mut self, target: ArithmeticTarget8, new_value: u8) {
        match target {
            ArithmeticTarget8::A => self.registers.a = new_value,
            ArithmeticTarget8::B => self.registers.b = new_value,
            ArithmeticTarget8::C => self.registers.c = new_value,
            ArithmeticTarget8::D => self.registers.d = new_value,
            ArithmeticTarget8::E => self.registers.e = new_value,
            ArithmeticTarget8::H => self.registers.h = new_value,
            ArithmeticTarget8::L => self.registers.l = new_value,
            ArithmeticTarget8::HL => self.registers.set_hl(new_value as u16),
            ArithmeticTarget8::Value(_) => (),
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
            ArithmeticTarget8::HL => self.registers.get_hl() as u8,
        }
    }
}

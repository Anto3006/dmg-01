mod registers;

use registers::Registers;

enum ArithmeticTarget {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
}

enum Instruction {
    ADD(ArithmeticTarget),
    ADC(ArithmeticTarget),
    SUB(ArithmeticTarget),
    SBC(ArithmeticTarget),
}

struct CPU {
    registers: Registers,
}

impl CPU {
    fn execute(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::ADD(target) => self.add(target, false),
            Instruction::ADC(target) => self.add(target, true),
            Instruction::SUB(target) => self.sub(target, false),
            Instruction::SBC(target) => self.sub(target, true),
            _ => (),
        }
    }

    fn add(&mut self, target: ArithmeticTarget, use_carry: bool) {
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

    fn sub(&mut self, target: ArithmeticTarget, use_carry: bool) {
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

    fn get_target_value(&self, target: ArithmeticTarget) -> u8 {
        match target {
            ArithmeticTarget::A => self.registers.a,
            ArithmeticTarget::B => self.registers.b,
            ArithmeticTarget::C => self.registers.c,
            ArithmeticTarget::D => self.registers.d,
            ArithmeticTarget::E => self.registers.e,
            ArithmeticTarget::H => self.registers.h,
            ArithmeticTarget::L => self.registers.l,
        }
    }
}

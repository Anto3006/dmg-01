pub mod register_types;
use register_types::{Register16Bit, Register8Bit};

const ZERO_FLAG_BIT: u8 = 7;
const SUB_FLAG_BIT: u8 = 6;
const HALF_CARRY_FLAG_BIT: u8 = 5;
const CARRY_FLAG_BIT: u8 = 4;

#[derive(Debug, Copy, Clone)]
pub struct Registers {
    a: u8,
    b: u8,
    c: u8,
    d: u8,
    e: u8,
    f: FlagsRegister,
    h: u8,
    l: u8,
    stack_pointer: u16,
    program_counter: u16,
}

#[derive(Debug, Copy, Clone)]
struct FlagsRegister {
    zero: bool,
    substraction: bool,
    half_carry: bool,
    carry: bool,
}

impl From<u8> for FlagsRegister {
    fn from(value: u8) -> Self {
        FlagsRegister {
            zero: value >> ZERO_FLAG_BIT == 1,
            substraction: value >> SUB_FLAG_BIT == 1,
            half_carry: value >> HALF_CARRY_FLAG_BIT == 1,
            carry: value >> CARRY_FLAG_BIT == 1,
        }
    }
}

impl From<FlagsRegister> for u8 {
    fn from(value: FlagsRegister) -> Self {
        let zero_flag_bit = if value.zero { 1 << ZERO_FLAG_BIT } else { 0 };
        let sub_flag_bit = if value.substraction {
            1 << SUB_FLAG_BIT
        } else {
            0
        };
        let half_carry_flag_bit = if value.half_carry {
            1 << HALF_CARRY_FLAG_BIT
        } else {
            0
        };
        let carry_flag_bit = if value.carry { 1 << CARRY_FLAG_BIT } else { 0 };
        zero_flag_bit | sub_flag_bit | half_carry_flag_bit | carry_flag_bit
    }
}

impl FlagsRegister {
    fn set_flags(&mut self, flag_results: FlagsResults) {
        if let Some(zero_flag) = flag_results.zero {
            self.zero = zero_flag;
        }
        if let Some(sub_flag) = flag_results.substraction {
            self.substraction = sub_flag;
        }
        if let Some(half_carry_flag) = flag_results.half_carry {
            self.half_carry = half_carry_flag;
        }
        if let Some(carry_flag) = flag_results.carry {
            self.carry = carry_flag;
        }
    }
}

pub struct FlagsResults {
    zero: Option<bool>,
    substraction: Option<bool>,
    half_carry: Option<bool>,
    carry: Option<bool>,
}

impl Default for FlagsResults {
    fn default() -> Self {
        Self {
            zero: None,
            substraction: None,
            half_carry: None,
            carry: None,
        }
    }
}

impl FlagsResults {
    pub fn new(
        zero: Option<bool>,
        substraction: Option<bool>,
        half_carry: Option<bool>,
        carry: Option<bool>,
    ) -> Self {
        Self {
            zero,
            substraction,
            half_carry,
            carry,
        }
    }
}

impl Registers {
    pub fn set_flags(&mut self, flag_results: FlagsResults) {
        self.f.set_flags(flag_results);
    }

    pub fn get_8_bit_register(&self, register: Register8Bit) -> u8 {
        match register {
            Register8Bit::A => self.a,
            Register8Bit::B => self.b,
            Register8Bit::C => self.c,
            Register8Bit::D => self.d,
            Register8Bit::E => self.e,
            Register8Bit::F => self.f.into(),
            Register8Bit::H => self.h,
            Register8Bit::L => self.l,
            Register8Bit::Addr => 0,
        }
    }

    pub fn get_16_bit_register(&self, register: Register16Bit) -> u16 {
        let (upper_register, lower_register) = match register {
            Register16Bit::AF => (Register8Bit::A, Register8Bit::F),
            Register16Bit::BC => (Register8Bit::B, Register8Bit::C),
            Register16Bit::DE => (Register8Bit::D, Register8Bit::E),
            Register16Bit::HL => (Register8Bit::H, Register8Bit::L),
            Register16Bit::SP => return self.stack_pointer,
        };
        let upper_half = self.get_8_bit_register(upper_register);
        let lower_half = self.get_8_bit_register(lower_register);
        (upper_half as u16) << 8 | (lower_half as u16)
    }

    pub fn set_8_bit_register(&mut self, register: Register8Bit, new_value: u8) {
        match register {
            Register8Bit::A => self.a = new_value,
            Register8Bit::B => self.b = new_value,
            Register8Bit::C => self.c = new_value,
            Register8Bit::D => self.d = new_value,
            Register8Bit::E => self.e = new_value,
            Register8Bit::F => self.f = new_value.into(),
            Register8Bit::H => self.h = new_value,
            Register8Bit::L => self.l = new_value,
            Register8Bit::Addr => (),
        }
    }

    pub fn set_16_bit_register(&mut self, register: Register16Bit, new_value: u16) {
        let (upper_register, lower_register) = match register {
            Register16Bit::AF => (Register8Bit::A, Register8Bit::F),
            Register16Bit::BC => (Register8Bit::B, Register8Bit::C),
            Register16Bit::DE => (Register8Bit::D, Register8Bit::E),
            Register16Bit::HL => (Register8Bit::H, Register8Bit::L),
            Register16Bit::SP => {
                self.stack_pointer = new_value;
                return;
            }
        };
        let upper_half = ((new_value & 0xFF00) >> 8) as u8;
        let lower_half = (new_value & 0x00FF) as u8;
        self.set_8_bit_register(upper_register, upper_half);
        self.set_8_bit_register(lower_register, lower_half);
    }

    pub fn get_program_counter(&self) -> u16 {
        self.program_counter
    }

    pub fn set_program_counter(&mut self, new_value: u16) {
        self.program_counter = new_value;
    }

    pub fn increase_program_counter(&mut self) {
        self.program_counter = self.program_counter.wrapping_add(1);
    }

    pub fn get_carry_flag(&self) -> bool {
        self.f.carry
    }

    pub fn get_sub_flag(&self) -> bool {
        self.f.substraction
    }

    pub fn get_half_carry_flag(&self) -> bool {
        self.f.half_carry
    }

    pub fn get_zero(&self) -> bool {
        self.f.zero
    }
}

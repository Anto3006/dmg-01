const ZERO_FLAG_BIT: u8 = 7;
const SUB_FLAG_BIT: u8 = 6;
const HALF_CARRY_FLAG_BIT: u8 = 5;
const CARRY_FLAG_BIT: u8 = 4;

enum Register8Bit {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
}

enum Register16Bit {
    AF,
    BC,
    DE,
    HL,
}

#[derive(Debug, Copy, Clone)]
struct Registers {
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

impl Registers {
    fn get_8_bit_register(&self, register: Register8Bit) -> u8 {
        match register {
            Register8Bit::A => self.a,
            Register8Bit::B => self.b,
            Register8Bit::C => self.c,
            Register8Bit::D => self.d,
            Register8Bit::E => self.e,
            Register8Bit::F => self.f.into(),
            Register8Bit::H => self.h,
            Register8Bit::L => self.l,
        }
    }

    fn get_16_bit_register(&self, register: Register16Bit) -> u16 {
        let (upper_register, lower_register) = match register {
            Register16Bit::AF => (Register8Bit::A, Register8Bit::F),
            Register16Bit::BC => (Register8Bit::B, Register8Bit::C),
            Register16Bit::DE => (Register8Bit::D, Register8Bit::E),
            Register16Bit::HL => (Register8Bit::H, Register8Bit::L),
        };
        let upper_half = self.get_8_bit_register(upper_register);
        let lower_half = self.get_8_bit_register(lower_register);
        (upper_half as u16) << 8 | (lower_half as u16)
    }

    fn set_8_bit_register(&mut self, register: Register8Bit, new_value: u8) {
        match register {
            Register8Bit::A => self.a = new_value,
            Register8Bit::B => self.b = new_value,
            Register8Bit::C => self.c = new_value,
            Register8Bit::D => self.d = new_value,
            Register8Bit::E => self.e = new_value,
            Register8Bit::F => self.f = new_value.into(),
            Register8Bit::H => self.h = new_value,
            Register8Bit::L => self.l = new_value,
        }
    }

    fn set_16_bit_register(&mut self, register: Register16Bit, new_value: u16) {
        let (upper_register, lower_register) = match register {
            Register16Bit::AF => (Register8Bit::A, Register8Bit::F),
            Register16Bit::BC => (Register8Bit::B, Register8Bit::C),
            Register16Bit::DE => (Register8Bit::D, Register8Bit::E),
            Register16Bit::HL => (Register8Bit::H, Register8Bit::L),
        };
        let upper_half = ((new_value & 0xFF00) >> 8) as u8;
        let lower_half = (new_value & 0x00FF) as u8;
        self.set_8_bit_register(upper_register, upper_half);
        self.set_8_bit_register(lower_register, lower_half);
    }

    fn get_stack_pointer(&self) -> u16 {
        self.stack_pointer
    }

    fn get_program_counter(&self) -> u16 {
        self.program_counter
    }
}

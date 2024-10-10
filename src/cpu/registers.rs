use std::convert;

pub struct Registers {
    pub a: u8,
    pub b: u8,
    pub c: u8,
    pub d: u8,
    pub e: u8,
    pub f: FlagsRegister,
    pub h: u8,
    pub l: u8,
}

const ZERO_FLAG_BYTE_POSITION: u8 = 7;
const SUBSTRACT_FLAG_BYTE_POSITION: u8 = 6;
const HALF_CARRY_FLAG_BYTE_POSITION: u8 = 5;
const CARRY_FLAG_BYTE_POSITION: u8 = 4;
#[derive(Debug, Copy, Clone)]
pub struct FlagsRegister {
    pub zero: bool,
    pub substract: bool,
    pub half_carry: bool,
    pub carry: bool,
}

impl convert::From<FlagsRegister> for u8 {
    fn from(value: FlagsRegister) -> Self {
        (if value.zero { 1 } else { 0 } << ZERO_FLAG_BYTE_POSITION)
            | (if value.substract { 1 } else { 0 } << SUBSTRACT_FLAG_BYTE_POSITION)
            | (if value.half_carry { 1 } else { 0 } << HALF_CARRY_FLAG_BYTE_POSITION)
            | (if value.carry { 1 } else { 0 } << CARRY_FLAG_BYTE_POSITION)
    }
}

impl convert::From<u8> for FlagsRegister {
    fn from(value: u8) -> Self {
        let zero = (value >> ZERO_FLAG_BYTE_POSITION) & 0b1 != 0;
        let substract = (value >> SUBSTRACT_FLAG_BYTE_POSITION) & 0b1 != 0;
        let half_carry = (value >> HALF_CARRY_FLAG_BYTE_POSITION) & 0b1 != 0;
        let carry = (value >> CARRY_FLAG_BYTE_POSITION) & 0b1 != 0;
        FlagsRegister {
            zero,
            substract,
            half_carry,
            carry,
        }
    }
}

impl Registers {
    pub fn get_bc(&self) -> u16 {
        let bc = (self.b as u16) << 8 | (self.c as u16);
        bc
    }

    pub fn set_bc(&mut self, new_value: u16) {
        self.b = ((new_value & 0xFF00) >> 8) as u8;
        self.c = (new_value & 0x00FF) as u8;
    }

    pub fn get_af(&self) -> u16 {
        let af = (self.a as u16) << 8 | (u8::from(self.f) as u16);
        af
    }

    pub fn set_af(&mut self, new_value: u16) {
        self.a = ((new_value & 0xFF00) >> 8) as u8;
        self.f = FlagsRegister::from((new_value & 0x00FF) as u8);
    }

    pub fn get_de(&self) -> u16 {
        let de = (self.d as u16) << 8 | (self.e as u16);
        de
    }

    pub fn set_de(&mut self, new_value: u16) {
        self.d = ((new_value & 0xFF00) >> 8) as u8;
        self.e = (new_value & 0x00FF) as u8;
    }

    pub fn get_hl(&self) -> u16 {
        let hl = (self.h as u16) << 8 | (self.l as u16);
        hl
    }

    pub fn set_hl(&mut self, new_value: u16) {
        self.h = ((new_value & 0xFF00) >> 8) as u8;
        self.l = (new_value & 0x00FF) as u8;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_16_bit_registers() {
        let registers = Registers {
            a: 0xF0,
            b: 0x1F,
            c: 0x05,
            d: 0x03,
            e: 0x00,
            f: FlagsRegister {
                zero: false,
                substract: true,
                half_carry: false,
                carry: true,
            },
            h: 0x11,
            l: 0x32,
        };
        assert_eq!(registers.get_bc(), 0x1F05);
        assert_eq!(registers.get_af(), 0xF050);
        assert_eq!(registers.get_de(), 0x0300);
        assert_eq!(registers.get_hl(), 0x1132);
    }

    #[test]
    fn test_set_16_bit_registers() {
        let mut registers = Registers {
            a: 0,
            b: 0,
            c: 0,
            d: 0,
            e: 0,
            f: FlagsRegister {
                zero: false,
                substract: false,
                half_carry: false,
                carry: false,
            },
            h: 0,
            l: 0,
        };
        registers.set_bc(0xFC05);
        registers.set_af(0x05AA);
        registers.set_de(0x65BC);
        registers.set_hl(0x1234);
        assert_eq!(registers.a, 0x05);
        assert_eq!(registers.b, 0xFC);
        assert_eq!(registers.c, 0x05);
        assert_eq!(registers.d, 0x65);
        assert_eq!(registers.e, 0xBC);
        assert_eq!(u8::from(registers.f), 0xA0);
        assert_eq!(registers.h, 0x12);
        assert_eq!(registers.l, 0x34);
    }
}

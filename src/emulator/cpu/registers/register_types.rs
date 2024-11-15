#[derive(Debug, Copy, Clone)]
pub enum Register8Bit {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
    L,
    Addr,
}

impl TryFrom<u8> for Register8Bit {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::B),
            1 => Ok(Self::C),
            2 => Ok(Self::D),
            3 => Ok(Self::E),
            4 => Ok(Self::H),
            5 => Ok(Self::L),
            6 => Ok(Self::Addr),
            7 => Ok(Self::A),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Register16Bit {
    AF,
    BC,
    DE,
    HL,
    SP,
}

impl TryFrom<u8> for Register16Bit {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::BC),
            1 => Ok(Self::DE),
            2 => Ok(Self::HL),
            3 => Ok(Self::SP),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Register {
    Reg8(Register8Bit),
    Reg16(Register16Bit),
}

impl From<Register8Bit> for Register {
    fn from(value: Register8Bit) -> Self {
        Self::Reg8(value)
    }
}

impl From<Register16Bit> for Register {
    fn from(value: Register16Bit) -> Self {
        Self::Reg16(value)
    }
}

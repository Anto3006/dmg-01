mod registers;

use super::mmu::MMU;
use register_types::{Register, Register16Bit, Register8Bit};
use registers::{register_types, FlagsResults, Registers};

#[derive(Debug, Clone, Copy)]
enum MemoryAddress {
    BC,
    DE,
    HLI,
    HLD,
}

impl TryFrom<u8> for MemoryAddress {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::BC),
            1 => Ok(Self::DE),
            2 => Ok(Self::HLI),
            3 => Ok(Self::HLD),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
    NOP,
    Load16Bit(Register16Bit, u16),
    StoreInMemory(MemoryAddress),  //Stores from register A
    LoadFromMemory(MemoryAddress), // Load to register A
    StoreStackPointer(u16),
    Increase(Register),
    Decrease(Register),
    Add16(Register16Bit), // Adds 16 bit register to HL register
    Load8Bit(Register8Bit, u8),
    RLCA,
    RRCA,
    RLA,
    RRA,
    Unknown(u8),
}

impl Instruction {
    fn show_debug_message(&self) {
        match self {
            Self::NOP => println!("No operation instruction"),
            Self::Load16Bit(register, value) => println!("Loading {register:?} with value {value}"),
            Self::StoreInMemory(memory_address) => {
                println!("Loading to memory address {memory_address:?}")
            }
            Self::LoadFromMemory(memory_address) => {
                println!("Loading from memory address {memory_address:?}")
            }
            Self::StoreStackPointer(address) => {
                println!("Stores stack pointer at address {address:#x}")
            }
            Self::Increase(register) => {
                println!("Increasing register {register:?}");
            }
            Self::Decrease(register) => {
                println!("Decreasing register {register:?}");
            }
            Self::Add16(register) => {
                println!("Adding {register:?} to HL");
            }
            Self::Load8Bit(register, value) => println!("Loading {register:?} with value {value}"),
            Self::RLCA => println!("Rotating register A to the left setting carry (RLCA)"),
            Self::RRCA => println!("Rotating register A to the right setting carry (RRCA)"),
            Self::RLA => println!("Rotating register A to the left through carry (RCA)"),
            Self::RRA => println!("Rotating register A to the right through carry (RCA)"),
            Self::Unknown(opcode) => println!("Unknown opcode: {opcode}"),
        }
    }
}

struct CPU {
    mmu: MMU,
    registers: Registers,
}

impl CPU {
    fn fetch_byte(&mut self) -> u8 {
        let byte = self.mmu.read_byte(self.registers.get_program_counter());
        self.registers.increase_program_counter();
        byte
    }

    fn decode_instruction(&mut self) -> Instruction {
        let opcode = self.fetch_byte();
        let instruction = self.decode_instruction_nibbles(opcode);
        if let Some(instruction) = instruction {
            return instruction;
        } else {
            let instruction = self.decode_instruction_octal(opcode);
            if let Some(instruction) = instruction {
                return instruction;
            } else {
                return Instruction::Unknown(opcode);
            }
        }
    }

    fn decode_instruction_nibbles(&mut self, opcode: u8) -> Option<Instruction> {
        let upper_nibble = opcode & 0xF0 >> 4;
        let lower_nibble = opcode & 0x0F;
        match (upper_nibble, lower_nibble) {
            (0..=3, 0b1) => {
                let register = Register16Bit::try_from(upper_nibble).unwrap();
                // Little endian
                let value = (self.fetch_byte() as u16) | ((self.fetch_byte() as u16) << 8);
                Some(Instruction::Load16Bit(register, value))
            }
            (0..=3, 0b10) => {
                let memory_address = MemoryAddress::try_from(upper_nibble).unwrap();
                Some(Instruction::StoreInMemory(memory_address))
            }
            (0..=3, 0b1010) => {
                let memory_address = MemoryAddress::try_from(upper_nibble).unwrap();
                Some(Instruction::LoadFromMemory(memory_address))
            }
            (0, 0b1000) => {
                let address = (self.fetch_byte() as u16) | ((self.fetch_byte() as u16) << 8);
                Some(Instruction::StoreStackPointer(address))
            }
            (0..4, 0b0011) => {
                let register = Register16Bit::try_from(upper_nibble).unwrap();
                Some(Instruction::Increase(Register::Reg16(register)))
            }
            (0..4, 0b1011) => {
                let register = Register16Bit::try_from(upper_nibble).unwrap();
                Some(Instruction::Decrease(Register::Reg16(register)))
            }
            (0..4, 0b1001) => {
                let register = Register16Bit::try_from(upper_nibble).unwrap();
                Some(Instruction::Add16(register))
            }
            (0, 0b0111) => Some(Instruction::RLCA),
            (0, 0b1111) => Some(Instruction::RRCA),
            (0b0001, 0b0111) => Some(Instruction::RLA),
            (0b0001, 0b1111) => Some(Instruction::RRA),
            _ => None,
        }
    }

    fn decode_instruction_octal(&mut self, opcode: u8) -> Option<Instruction> {
        let low_octal = opcode & 0o7;
        let middle_octal = opcode & 0o70;
        let top_octal = opcode & 0o300;
        match (top_octal, middle_octal, low_octal) {
            (0, _, 0b100) => {
                let register = Register::Reg8(Register8Bit::try_from(middle_octal).unwrap());
                Some(Instruction::Increase(register))
            }
            (0, _, 0b101) => {
                let register = Register::Reg8(Register8Bit::try_from(middle_octal).unwrap());
                Some(Instruction::Decrease(register))
            }
            (0, _, 0b110) => {
                let register = Register8Bit::try_from(middle_octal).unwrap();
                let value = self.fetch_byte();
                Some(Instruction::Load8Bit(register, value))
            }
            _ => None,
        }
    }

    fn execute_instruction(&mut self, instruction: Instruction) {
        let flags_results = match instruction {
            Instruction::NOP => FlagsResults::default(),
            Instruction::Load16Bit(register, value) => self.load_register_16(register, value),
            Instruction::StoreInMemory(memory_address) => self.store_in_memory(memory_address),
            Instruction::LoadFromMemory(memory_address) => self.load_from_memory(memory_address),
            Instruction::StoreStackPointer(address) => self.store_stack_pointer(address),
            Instruction::Increase(register) => self.increase_register(register),
            Instruction::Decrease(register) => self.decrease_register(register),
            Instruction::Add16(register) => self.add_register_16(register, Register16Bit::HL),
            Instruction::Load8Bit(register, value) => self.load_register_8(register, value),
            Instruction::RLCA => self.rotate_left_set_carry(Register8Bit::A),
            Instruction::RRCA => self.rotate_right_set_carry(Register8Bit::A),
            Instruction::RLA => self.rotate_left_set_carry(Register8Bit::A),
            Instruction::RRA => self.rotate_right_set_carry(Register8Bit::A),
            Instruction::Unknown(_) => FlagsResults::default(),
        };
        self.registers.set_flags(flags_results);
    }

    fn from_memory_address(&mut self, memory_address: MemoryAddress) -> u16 {
        match memory_address {
            MemoryAddress::BC => self.registers.get_16_bit_register(Register16Bit::BC),
            MemoryAddress::DE => self.registers.get_16_bit_register(Register16Bit::DE),
            MemoryAddress::HLD => {
                let value = self.registers.get_16_bit_register(Register16Bit::HL);
                self.registers
                    .set_16_bit_register(Register16Bit::HL, value.wrapping_sub(1));
                value
            }
            MemoryAddress::HLI => {
                let value = self.registers.get_16_bit_register(Register16Bit::HL);
                self.registers
                    .set_16_bit_register(Register16Bit::HL, value.wrapping_add(1));
                value
            }
        }
    }

    fn get_register_8_value(&self, register: Register8Bit) -> u8 {
        if let Register8Bit::Addr = register {
            let addr = self.registers.get_16_bit_register(Register16Bit::HL);
            self.mmu.read_byte(addr)
        } else {
            self.registers.get_8_bit_register(register)
        }
    }

    fn set_register_8_value(&mut self, register: Register8Bit, value: u8) {
        if let Register8Bit::Addr = register {
            let addr = self.registers.get_16_bit_register(Register16Bit::HL);
            self.mmu.write_byte(addr, value);
        } else {
            self.registers.set_8_bit_register(register, value);
        }
    }

    fn load_register_16(&mut self, register: Register16Bit, value: u16) -> FlagsResults {
        self.registers.set_16_bit_register(register, value);
        FlagsResults::default()
    }

    fn load_register_8(&mut self, register: Register8Bit, value: u8) -> FlagsResults {
        self.set_register_8_value(register, value);
        FlagsResults::default()
    }

    fn store_in_memory(&mut self, memory_address: MemoryAddress) -> FlagsResults {
        let value = self.registers.get_8_bit_register(Register8Bit::A);
        let address = self.from_memory_address(memory_address);
        self.mmu.write_byte(address, value);
        FlagsResults::default()
    }

    fn store_stack_pointer(&mut self, address: u16) -> FlagsResults {
        let sp = self.registers.get_16_bit_register(Register16Bit::SP);
        let upper_half = (sp >> 8) as u8;
        let lower_half = (sp & 0x00FF) as u8;
        self.mmu.write_byte(address, lower_half);
        self.mmu.write_byte(address.wrapping_add(1), upper_half);
        FlagsResults::default()
    }

    fn load_from_memory(&mut self, memory_address: MemoryAddress) -> FlagsResults {
        let address = self.from_memory_address(memory_address);
        let value = self.mmu.read_byte(address);
        self.registers.set_8_bit_register(Register8Bit::A, value);
        FlagsResults::default()
    }

    fn increase_register(&mut self, register: Register) -> FlagsResults {
        match register {
            Register::Reg8(register) => {
                let value = self.get_register_8_value(register);
                let new_value = value.wrapping_add(1);
                self.set_register_8_value(register, value);
                let zero = Some(new_value == 0);
                let substraction = Some(false);
                let half_carry = Some(Self::did_half_carry_add_8(value, 1));
                let carry = None;
                let flag_results = FlagsResults::new(zero, substraction, half_carry, carry);
                flag_results
            }

            Register::Reg16(register) => {
                let value = self.registers.get_16_bit_register(register);
                let new_value = value.wrapping_add(1);
                self.registers.set_16_bit_register(register, new_value);
                FlagsResults::default()
            }
        }
    }

    fn decrease_register(&mut self, register: Register) -> FlagsResults {
        match register {
            Register::Reg8(register) => {
                let value = self.get_register_8_value(register);
                let new_value = value.wrapping_sub(1);
                self.set_register_8_value(register, value);
                let zero = Some(new_value == 0);
                let substraction = Some(true);
                let half_carry = Some(Self::did_half_carry_sub_8(value, 1));
                let carry = None;
                let flag_results = FlagsResults::new(zero, substraction, half_carry, carry);
                flag_results
            }

            Register::Reg16(register) => {
                let value = self.registers.get_16_bit_register(register);
                let new_value = value.wrapping_sub(1);
                self.registers.set_16_bit_register(register, new_value);
                FlagsResults::default()
            }
        }
    }

    fn add_register_16(&mut self, source: Register16Bit, dest: Register16Bit) -> FlagsResults {
        let source_value = self.registers.get_16_bit_register(source);
        let dest_value = self.registers.get_16_bit_register(dest);
        let (new_value, did_overflow) = dest_value.overflowing_add(source_value);
        self.registers.set_16_bit_register(dest, new_value);
        let did_half_carry = Self::did_half_carry_add_16(dest_value, source_value);
        let zero = Some(false);
        let sub = None;
        let half_carry = Some(did_half_carry);
        let carry = Some(did_overflow);
        let flag_results = FlagsResults::new(zero, sub, half_carry, carry);
        flag_results
    }

    fn rotate_left_set_carry(&mut self, register: Register8Bit) -> FlagsResults {
        let mask = 0b1000_0000;
        let value = self.get_register_8_value(register);
        let did_overflow = (value & mask) == mask;
        let new_value = value.rotate_left(1);
        let zero;
        let substraction = Some(false);
        let half_carry = Some(false);
        let carry = Some(did_overflow);
        self.set_register_8_value(register, value);
        if let Register8Bit::A = register {
            zero = Some(false);
        } else {
            zero = Some(new_value == 0);
        }
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn rotate_right_set_carry(&mut self, register: Register8Bit) -> FlagsResults {
        let mask = 0b0000_0001;
        let value = self.get_register_8_value(register);
        let did_overflow = (value & mask) == mask;
        let new_value = value.rotate_right(1);
        let zero;
        let substraction = Some(false);
        let half_carry = Some(false);
        let carry = Some(did_overflow);
        self.set_register_8_value(register, value);
        if let Register8Bit::A = register {
            zero = Some(false);
        } else {
            zero = Some(new_value == 0);
        }
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn rotate_left_through_carry(&mut self, register: Register8Bit) -> FlagsResults {
        let mask = 0b1000_0000;
        let value = self.get_register_8_value(register);
        let did_overflow = (value & mask) == mask;
        let new_value = (value << 1) | (self.registers.get_carry_flag() as u8);
        let zero;
        let substraction = Some(false);
        let half_carry = Some(false);
        let carry = Some(did_overflow);
        self.set_register_8_value(register, value);
        if let Register8Bit::A = register {
            zero = Some(false);
        } else {
            zero = Some(new_value == 0);
        }
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn rotate_right_through_carry(&mut self, register: Register8Bit) -> FlagsResults {
        let mask = 0b0000_0001;
        let value = self.get_register_8_value(register);
        let did_overflow = (value & mask) == mask;
        let new_value = (value >> 1) | ((self.registers.get_carry_flag() as u8) << 7);
        let zero;
        let substraction = Some(false);
        let half_carry = Some(false);
        let carry = Some(did_overflow);
        self.set_register_8_value(register, value);
        if let Register8Bit::A = register {
            zero = Some(false);
        } else {
            zero = Some(new_value == 0);
        }
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn did_half_carry_add_16(value_1: u16, value_2: u16) -> bool {
        let half_carry_mask = 0x0FFF;
        let did_half_carry =
            (value_1 & half_carry_mask) + (value_2 & half_carry_mask) > half_carry_mask;
        did_half_carry
    }

    fn did_half_carry_add_8(value_1: u8, value_2: u8) -> bool {
        let half_carry_mask = 0x0F;
        let did_half_carry =
            (value_1 & half_carry_mask) + (value_2 & half_carry_mask) > half_carry_mask;
        did_half_carry
    }

    fn did_half_carry_sub_8(value_1: u8, value_2: u8) -> bool {
        let half_carry_mask = 0x0F;
        let did_half_carry = (value_1 & half_carry_mask) < (value_2 & half_carry_mask);
        did_half_carry
    }
}

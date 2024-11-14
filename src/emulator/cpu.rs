mod registers;

use super::mmu::MMU;
use registers::{Register, Register16Bit, Register8Bit, Registers};

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
        let upper_nibble = opcode & 0xF0 >> 4;
        let lower_nibble = opcode & 0x0F;
        match (upper_nibble, lower_nibble) {
            (0..=3, 0b1) => {
                let register = Register16Bit::try_from(upper_nibble).unwrap();
                // Little endian
                let value = (self.fetch_byte() as u16) | ((self.fetch_byte() as u16) << 8);
                Instruction::Load16Bit(register, value)
            }
            (0..=3, 0b10) => {
                let memory_address = MemoryAddress::try_from(upper_nibble).unwrap();
                Instruction::StoreInMemory(memory_address)
            }
            (0..=3, 0b1010) => {
                let memory_address = MemoryAddress::try_from(upper_nibble).unwrap();
                Instruction::LoadFromMemory(memory_address)
            }
            (0, 0b1000) => {
                let address = (self.fetch_byte() as u16) | ((self.fetch_byte() as u16) << 8);
                Instruction::StoreStackPointer(address)
            }
            (0..4, 0b0011) => {
                let register = Register16Bit::try_from(upper_nibble).unwrap();
                Instruction::Increase(Register::Reg16(register))
            }
            (0..4, 0b1011) => {
                let register = Register16Bit::try_from(upper_nibble).unwrap();
                Instruction::Decrease(Register::Reg16(register))
            }
            (0..4, 0b1001) => {
                let register = Register16Bit::try_from(upper_nibble).unwrap();
                Instruction::Add16(register)
            }
            _ => Instruction::Unknown(opcode),
        }
    }

    fn execute_instruction(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::NOP => (),
            Instruction::Load16Bit(register, value) => {
                self.registers.set_16_bit_register(register, value)
            }
            Instruction::StoreInMemory(memory_address) => {
                let value = self.registers.get_8_bit_register(Register8Bit::A);
                let address = self.from_memory_address(memory_address);
                self.mmu.write_byte(address, value);
            }
            Instruction::LoadFromMemory(memory_address) => {
                let address = self.from_memory_address(memory_address);
                let value = self.mmu.read_byte(address);
                self.registers.set_8_bit_register(Register8Bit::A, value);
            }
            Instruction::StoreStackPointer(address) => {
                let sp = self.registers.get_16_bit_register(Register16Bit::SP);
                let upper_half = (sp >> 8) as u8;
                let lower_half = (sp & 0x00FF) as u8;
                self.mmu.write_byte(address, lower_half);
                self.mmu.write_byte(address.wrapping_add(1), upper_half);
            }
            Instruction::Increase(register) => self.registers.increase_register(register),
            Instruction::Decrease(register) => self.registers.decrease_register(register),
            Instruction::Add16(register) => {}
            Instruction::Unknown(_) => (),
        }
    }

    fn from_memory_address(&mut self, memory_address: MemoryAddress) -> u16 {
        match memory_address {
            MemoryAddress::BC => self.registers.get_16_bit_register(Register16Bit::BC),
            MemoryAddress::DE => self.registers.get_16_bit_register(Register16Bit::DE),
            MemoryAddress::HLD => {
                let value = self.registers.get_16_bit_register(Register16Bit::HL);
                self.registers
                    .decrease_register(Register::Reg16(Register16Bit::HL));
                value
            }
            MemoryAddress::HLI => {
                let value = self.registers.get_16_bit_register(Register16Bit::HL);
                self.registers
                    .increase_register(Register::Reg16(Register16Bit::HL));
                value
            }
        }
    }
}

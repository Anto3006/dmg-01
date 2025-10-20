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
enum StackRegister {
    BC,
    DE,
    HL,
    AF,
}

impl TryFrom<u8> for StackRegister {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::BC),
            1 => Ok(Self::DE),
            2 => Ok(Self::HL),
            3 => Ok(Self::AF),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum Condition {
    Zero,
    NonZero,
    Carry,
    NonCarry,
}

impl TryFrom<u8> for Condition {
    type Error = ();
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::NonZero),
            1 => Ok(Self::Zero),
            2 => Ok(Self::NonCarry),
            3 => Ok(Self::Carry),
            _ => Err(()),
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum ArithmeticTarget {
    Register(Register8Bit),
    Value(u8),
}

#[derive(Debug, Copy, Clone)]
enum AccMemoryTarget {
    RegC,
    Imm8(u8),
    Imm16(u16),
}

#[derive(Debug, Clone, Copy)]
enum Instruction {
    NOP,                                      //nop
    Load16Bit(Register16Bit, u16),            //ld r16, imm16
    StoreInMemory(MemoryAddress),             //ld [r16mem], a. Stores from register A
    LoadFromMemory(MemoryAddress),            //ld a, [r16mem]. Load to register A
    StoreStackPointer(u16),                   //ld [imm16], sp
    Increase(Register),                       //inc r16 | inc r8
    Decrease(Register),                       //dec r16 | dec r8
    Add16(Register16Bit),                     //add hl, r16. Adds 16 bit register to HL register
    Load8Bit(Register8Bit, u8),               //ld r8, imm8
    RLCA,                                     //rlca
    RRCA,                                     //rrca
    RLA,                                      //rla
    RRA,                                      //rra
    DAA,                                      //daa
    CPL,                                      //cpl
    SCF,                                      //scf
    CCF,                                      //ccf
    RelativeJump(i8),                         //jr imm8
    CondRelativeJump(Condition, i8),          //jr cond, imm8
    Stop,                                     //stop
    RegisterLoad(Register8Bit, Register8Bit), //ld r8, r8
    Halt,                                     //halt
    Add(ArithmeticTarget), //add a, r8 | add a, imm8. Arithmetic operations with 8 bits have as target register A
    AddWithCarry(ArithmeticTarget), //adc a, r8 | adc a, imm8
    Sub(ArithmeticTarget), //sub a, r8 | sub a, imm8
    SubWithCarry(ArithmeticTarget), //sbc a, r8 | sbc a, r8
    AND(ArithmeticTarget), //and a, r8 | and a, imm8
    XOR(ArithmeticTarget), //xor a, r8 | xor a, imm8
    OR(ArithmeticTarget),  //or a, r8 | or a, imm8
    Compare(ArithmeticTarget), //cp a, r8 | cp a, imm8
    Ret,                   //ret
    RetCondition(Condition), //ret cond
    RetInterrupt,          //reti
    Jump(u16),             //jp imm16
    JumpCond(Condition, u16), //jp cond, imm16
    JumpHL,                //jp hl
    CallCondition(Condition, u16), //call cond, imm16
    Call(u16),             //call imm16
    Restart(u8),           //rst tgt3
    Pop(StackRegister),    //pop r16stk
    Push(StackRegister),   //push r16stk
    LoadToMemoryFromAcc(AccMemoryTarget), //ldh [c],a | ldh [imm8], a | ld [imm16], a
    LoadToAccFromMemory(AccMemoryTarget), //ldh [c],a | ldh [imm8], a | ld [imm16], a
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
            Self::DAA => println!("Decimal adjust accumulator"),
            Self::CPL => println!("Complement accumulator"),
            Self::SCF => println!("Set carry flag"),
            Self::CCF => println!("Complement carry flag"),
            Self::RelativeJump(offset) => println!("Relative jump with offset {offset}"),
            Self::CondRelativeJump(condition, offset) => {
                println!("Conditional relative jump with condition {condition:?} offset {offset}")
            }
            Self::Stop => println!("Stop operation"),
            Self::RegisterLoad(source, dest) => {
                println!("Loading value in register {source:?} to register {dest:?}")
            }
            Self::Halt => println!("Halt operation"),
            Self::Add(target) => println!("Adding the value in {target:?} to register A"),
            Self::AddWithCarry(target) => {
                println!("Adding the value in {target:?} plus the carry flag to register A")
            }
            Self::Sub(target) => println!("Substracting the value in {target:?} from register A"),
            Self::SubWithCarry(target) => {
                println!("Substracting the value in {target:?} plus the carry flag from register A")
            }
            Self::AND(target) => {
                println!("Executing bitwise AND with the value in {target:?} on the register A")
            }
            Self::XOR(target) => {
                println!("Executing bitwise XOR with the value in {target:?} on the register A")
            }
            Self::OR(target) => {
                println!("Executing bitwise OR with the value in {target:?} on the register A")
            }
            Self::Compare(target) => {
                println!("Comparing the value in {target:?} with the value in register A")
            }
            Self::Ret => {
                println!("Returning from a subroutine")
            }
            Self::RetCondition(condition) => {
                println!("Returning from a subroutine if {condition:?} is met")
            }
            Self::RetInterrupt => {
                println!("Returning from subrountine and enabling interrupts")
            }
            Self::Jump(address) => {
                println!("Jumping to address {address:#x}")
            }
            Self::JumpCond(condition, address) => {
                println!("Jump to address {address:#x} if condition {condition:?} is met")
            }
            Self::JumpHL => {
                println!("Jump to address in register HL")
            }
            Self::CallCondition(condition, addr) => {
                println!("Call to address {addr:#x} if condition {condition:?} is met")
            }
            Self::Call(addr) => {
                println!("Call to address {addr:#x}")
            }
            Self::Restart(target) => {
                println!("Restart to address defined by {target:#x}")
            }
            Self::Pop(register) => {
                println!("Pop data from stack to register {register:?}")
            }
            Self::Push(register) => {
                println!("Push data to the stack from register {register:?}")
            }
            Self::LoadToMemoryFromAcc(memory_target) => {
                println!("Loading to {memory_target:?} from accumulator")
            }
            Self::LoadToAccFromMemory(memory_target) => {
                println!("Loading to accumulator from {memory_target:?}")
            }
            Self::Unknown(opcode) => println!("Unknown opcode: {opcode}"),
        }
    }
}

struct CPU {
    mmu: MMU,
    registers: Registers,
    interrupt_flag_prepared: bool,
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
            (0b0010, 0b0111) => Some(Instruction::DAA),
            (0b0010, 0b1111) => Some(Instruction::CPL),
            (0b0011, 0b0111) => Some(Instruction::SCF),
            (0b0011, 0b1111) => Some(Instruction::CCF),
            (0b0001, 0b1000) => {
                let offset: i8 = self.fetch_byte() as i8;
                Some(Instruction::RelativeJump(offset))
            }
            (1, 0) => Some(Instruction::Stop),
            (0b0111, 0b0110) => Some(Instruction::Halt),
            (0b1100, 0b0110) => {
                let value = self.fetch_byte();
                Some(Instruction::Add(ArithmeticTarget::Value(value)))
            }
            (0b1100, 0b1110) => {
                let value = self.fetch_byte();
                Some(Instruction::AddWithCarry(ArithmeticTarget::Value(value)))
            }
            (0b1101, 0b0110) => {
                let value = self.fetch_byte();
                Some(Instruction::Sub(ArithmeticTarget::Value(value)))
            }
            (0b1101, 0b1110) => {
                let value = self.fetch_byte();
                Some(Instruction::SubWithCarry(ArithmeticTarget::Value(value)))
            }
            (0b1110, 0b0110) => {
                let value = self.fetch_byte();
                Some(Instruction::AND(ArithmeticTarget::Value(value)))
            }
            (0b1110, 0b1110) => {
                let value = self.fetch_byte();
                Some(Instruction::XOR(ArithmeticTarget::Value(value)))
            }
            (0b1111, 0b0110) => {
                let value = self.fetch_byte();
                Some(Instruction::OR(ArithmeticTarget::Value(value)))
            }
            (0b1111, 0b1110) => {
                let value = self.fetch_byte();
                Some(Instruction::Compare(ArithmeticTarget::Value(value)))
            }
            (0b1100, 0b1001) => Some(Instruction::Ret),
            (0b1101, 0b1001) => Some(Instruction::RetInterrupt),
            (0b1100, 0b0011) => {
                // Little endian
                let address = (self.fetch_byte() as u16) | ((self.fetch_byte() as u16) << 8);
                Some(Instruction::Jump(address))
            }
            (0b1110, 0b1001) => Some(Instruction::JumpHL),
            (0b1100, 0b1101) => {
                // Little endian
                let new_address = (self.fetch_byte() as u16) | ((self.fetch_byte() as u16) << 8);
                Some(Instruction::Call(new_address))
            }
            (0b1100..0b1111, 0b0001) => {
                let register = StackRegister::try_from(upper_nibble & 0x0F).unwrap();
                Some(Instruction::Pop(register))
            }
            (0b1100..0b1111, 0b0101) => {
                let register = StackRegister::try_from(upper_nibble & 0x0F).unwrap();
                Some(Instruction::Push(register))
            }
            (0b1110, 0b0010) => Some(Instruction::LoadToMemoryFromAcc(AccMemoryTarget::RegC)),
            (0b1110, 0b0000) => {
                let address_lsb = self.fetch_byte();
                Some(Instruction::LoadToMemoryFromAcc(AccMemoryTarget::Imm8(
                    address_lsb,
                )))
            }
            (0b1110, 0b1010) => {
                let address = (self.fetch_byte() as u16) | ((self.fetch_byte() as u16) << 8);
                Some(Instruction::LoadToMemoryFromAcc(AccMemoryTarget::Imm16(
                    address,
                )))
            }
            (0b1111, 0b0010) => Some(Instruction::LoadToAccFromMemory(AccMemoryTarget::RegC)),
            (0b1111, 0b0000) => {
                let address_lsb = self.fetch_byte();
                Some(Instruction::LoadToAccFromMemory(AccMemoryTarget::Imm8(
                    address_lsb,
                )))
            }
            (0b1111, 0b1010) => {
                let address = (self.fetch_byte() as u16) | ((self.fetch_byte() as u16) << 8);
                Some(Instruction::LoadToAccFromMemory(AccMemoryTarget::Imm16(
                    address,
                )))
            }
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
            (0, 4..8, 0) => {
                let condition_code = middle_octal & 0b11;
                let condition = Condition::try_from(condition_code).unwrap();
                let offset: i8 = self.fetch_byte() as i8;
                Some(Instruction::CondRelativeJump(condition, offset))
            }
            (0b01, _, _) => {
                let source_register = Register8Bit::try_from(middle_octal).unwrap();
                let dest_register = Register8Bit::try_from(low_octal).unwrap();
                Some(Instruction::RegisterLoad(source_register, dest_register))
            }
            (0b10, 0b000, _) => {
                let register = Register8Bit::try_from(low_octal).unwrap();
                Some(Instruction::Add(ArithmeticTarget::Register(register)))
            }
            (0b10, 0b001, _) => {
                let register = Register8Bit::try_from(low_octal).unwrap();
                Some(Instruction::AddWithCarry(ArithmeticTarget::Register(
                    register,
                )))
            }
            (0b10, 0b010, _) => {
                let register = Register8Bit::try_from(low_octal).unwrap();
                Some(Instruction::Sub(ArithmeticTarget::Register(register)))
            }
            (0b10, 0b011, _) => {
                let register = Register8Bit::try_from(low_octal).unwrap();
                Some(Instruction::SubWithCarry(ArithmeticTarget::Register(
                    register,
                )))
            }
            (0b10, 0b100, _) => {
                let register = Register8Bit::try_from(low_octal).unwrap();
                Some(Instruction::AND(ArithmeticTarget::Register(register)))
            }
            (0b10, 0b101, _) => {
                let register = Register8Bit::try_from(low_octal).unwrap();
                Some(Instruction::XOR(ArithmeticTarget::Register(register)))
            }
            (0b10, 0b110, _) => {
                let register = Register8Bit::try_from(low_octal).unwrap();
                Some(Instruction::OR(ArithmeticTarget::Register(register)))
            }
            (0b10, 0b111, _) => {
                let register = Register8Bit::try_from(low_octal).unwrap();
                Some(Instruction::Compare(ArithmeticTarget::Register(register)))
            }
            (0b11, 0..4, 0b000) => {
                let condition = Condition::try_from(middle_octal).unwrap();
                Some(Instruction::RetCondition(condition))
            }
            (0b11, 0..4, 0b010) => {
                // Little endian
                let address = (self.fetch_byte() as u16) | ((self.fetch_byte() as u16) << 8);
                let condition = Condition::try_from(middle_octal).unwrap();
                Some(Instruction::JumpCond(condition, address))
            }
            (0b11, 0..4, 0b100) => {
                // Little endian
                let new_address = (self.fetch_byte() as u16) | ((self.fetch_byte() as u16) << 8);
                let condition = Condition::try_from(middle_octal).unwrap();
                Some(Instruction::CallCondition(condition, new_address))
            }
            (0b11, _, 0b111) => Some(Instruction::Restart(middle_octal)),

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
            Instruction::DAA => self.decimal_adjust_accumulator(),
            Instruction::CPL => self.complement_accumulator(),
            Instruction::SCF => self.set_carry_flag(),
            Instruction::CCF => self.complement_carry_flag(),
            Instruction::RelativeJump(offset) => self.relative_jump(offset),
            Instruction::CondRelativeJump(condition, offset) => {
                self.conditional_relative_jump(condition, offset)
            }
            Instruction::Stop => todo!(),
            Instruction::RegisterLoad(source, dest) => self.register_load(source, dest),
            Instruction::Halt => todo!(),
            Instruction::Add(target) => self.add_8(target),
            Instruction::AddWithCarry(target) => self.add_8_with_carry(target),
            Instruction::Sub(target) => self.sub_8(target),
            Instruction::SubWithCarry(target) => self.sub_8_with_carry(target),
            Instruction::AND(target) => self.and(target),
            Instruction::XOR(target) => self.xor(target),
            Instruction::OR(target) => self.or(target),
            Instruction::Compare(target) => self.compare(target),
            Instruction::Ret => self.return_subroutine(),
            Instruction::RetCondition(condition) => self.conditional_return_subroutine(condition),
            Instruction::RetInterrupt => self.return_subroutine_interrupt(),
            Instruction::Jump(address) => self.jump(address),
            Instruction::JumpCond(condition, address) => self.jump_conditional(condition, address),
            Instruction::JumpHL => self.jump_hl(),
            Instruction::Call(address) => self.call(address),
            Instruction::CallCondition(condition, address) => {
                self.conditional_call(condition, address)
            }
            Instruction::Restart(target) => self.restart(target),
            Instruction::Pop(register) => self.pop_stack(register),
            Instruction::Push(register) => self.push_stack(register),
            Instruction::LoadToMemoryFromAcc(acc_memory_target) => {
                self.load_to_mem_from_acc(acc_memory_target)
            }
            Instruction::LoadToAccFromMemory(acc_memory_target) => {
                self.load_to_acc_from_mem(acc_memory_target)
            }
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

    fn decimal_adjust_accumulator(&mut self) -> FlagsResults {
        let value = self.get_register_8_value(Register8Bit::A);
        let sub_flag = self.registers.get_sub_flag();
        let half_carry_flag = self.registers.get_half_carry_flag();
        let carry_flag = self.registers.get_carry_flag();

        let mut correction = 0;
        if half_carry_flag || (!sub_flag && (value & 0x0F) > 9) {
            correction |= 0x6;
        }
        if carry_flag || (!sub_flag && value > 0x99) {
            correction |= 0x60;
        }

        let new_value = if sub_flag {
            value.wrapping_sub(correction)
        } else {
            value.wrapping_add(correction)
        };

        let zero = Some(new_value == 0);
        let new_carry_flag = Some(value > 0x99);
        let new_half_carry_flag = Some(false);
        let new_sub_flag = None;

        FlagsResults::new(zero, new_sub_flag, new_half_carry_flag, new_carry_flag)
    }

    fn complement_accumulator(&mut self) -> FlagsResults {
        let value = self.get_register_8_value(Register8Bit::A);
        let new_value = !value;
        self.set_register_8_value(Register8Bit::A, new_value);
        let zero = None;
        let substraction = Some(true);
        let half_carry = Some(true);
        let carry = None;
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn set_carry_flag(&mut self) -> FlagsResults {
        let zero = None;
        let substraction = Some(false);
        let half_carry = Some(false);
        let carry = Some(true);
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn complement_carry_flag(&mut self) -> FlagsResults {
        let zero = None;
        let substraction = Some(false);
        let half_carry = Some(false);
        let carry = Some(!self.registers.get_carry_flag());
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn relative_jump(&mut self, offset: i8) -> FlagsResults {
        let program_counter = self.registers.get_program_counter();
        let new_program_counter = if offset >= 0 {
            program_counter.wrapping_add(offset as u16)
        } else {
            program_counter.wrapping_sub((-offset) as u16)
        };
        self.registers.set_program_counter(new_program_counter);
        FlagsResults::default()
    }

    fn conditional_relative_jump(&mut self, condition: Condition, offset: i8) -> FlagsResults {
        if self.check_condition(condition) {
            self.relative_jump(offset);
        }
        FlagsResults::default()
    }

    fn register_load(&mut self, source: Register8Bit, dest: Register8Bit) -> FlagsResults {
        let value = self.get_register_8_value(source);
        self.load_register_8(dest, value);
        FlagsResults::default()
    }

    fn add_8(&mut self, target: ArithmeticTarget) -> FlagsResults {
        let acc_value = self.get_register_8_value(Register8Bit::A);
        let value;
        match target {
            ArithmeticTarget::Register(register) => value = self.get_register_8_value(register),
            ArithmeticTarget::Value(v) => value = v,
        }
        let (new_value, did_overflow) = acc_value.overflowing_add(value);
        self.set_register_8_value(Register8Bit::A, new_value);
        let zero = Some(new_value == 0);
        let substraction = Some(false);
        let half_carry = Some(Self::did_half_carry_add_8(acc_value, value));
        let carry = Some(did_overflow);
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn add_8_with_carry(&mut self, target: ArithmeticTarget) -> FlagsResults {
        let acc_value = self.get_register_8_value(Register8Bit::A);
        let carry_value = self.registers.get_carry_flag() as u8;
        let value;
        match target {
            ArithmeticTarget::Register(register) => value = self.get_register_8_value(register),
            ArithmeticTarget::Value(v) => value = v,
        }
        let (value_with_carry, did_value_overflow) = value.overflowing_add(carry_value);
        let (new_value, did_overflow) = acc_value.overflowing_add(value_with_carry);
        let did_overflow = did_overflow || did_value_overflow;
        self.set_register_8_value(Register8Bit::A, new_value);
        let zero = Some(new_value == 0);
        let substraction = Some(false);
        let half_carry = Some(Self::did_half_carry_add_8(acc_value, new_value));
        let carry = Some(did_overflow);
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn sub_8(&mut self, target: ArithmeticTarget) -> FlagsResults {
        let acc_value = self.get_register_8_value(Register8Bit::A);
        let value;
        match target {
            ArithmeticTarget::Register(register) => value = self.get_register_8_value(register),
            ArithmeticTarget::Value(v) => value = v,
        }
        let (new_value, did_overflow) = acc_value.overflowing_sub(value);
        self.set_register_8_value(Register8Bit::A, new_value);
        let zero = Some(new_value == 0);
        let substraction = Some(true);
        let half_carry = Some(Self::did_half_carry_sub_8(acc_value, value));
        let carry = Some(did_overflow);
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn sub_8_with_carry(&mut self, target: ArithmeticTarget) -> FlagsResults {
        let acc_value = self.get_register_8_value(Register8Bit::A);
        let carry_value = self.registers.get_carry_flag() as u8;
        let value;
        match target {
            ArithmeticTarget::Register(register) => value = self.get_register_8_value(register),
            ArithmeticTarget::Value(v) => value = v,
        }
        let (value_with_carry, did_value_overflow) = value.overflowing_add(carry_value);
        let (new_value, did_overflow) = acc_value.overflowing_sub(value_with_carry);
        let did_overflow = did_overflow || did_value_overflow;
        self.set_register_8_value(Register8Bit::A, new_value);
        let zero = Some(new_value == 0);
        let substraction = Some(true);
        let half_carry = Some(Self::did_half_carry_sub_8(acc_value, new_value));
        let carry = Some(did_overflow);
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn and(&mut self, target: ArithmeticTarget) -> FlagsResults {
        let acc_value = self.get_register_8_value(Register8Bit::A);
        let value = match target {
            ArithmeticTarget::Register(register) => self.get_register_8_value(register),
            ArithmeticTarget::Value(v) => v,
        };
        let new_value = acc_value & value;
        self.set_register_8_value(Register8Bit::A, new_value);
        let zero = Some(new_value == 0);
        let substraction = Some(false);
        let half_carry = Some(true);
        let carry = Some(false);
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn xor(&mut self, target: ArithmeticTarget) -> FlagsResults {
        let acc_value = self.get_register_8_value(Register8Bit::A);
        let value = match target {
            ArithmeticTarget::Register(register) => self.get_register_8_value(register),
            ArithmeticTarget::Value(v) => v,
        };
        let new_value = acc_value ^ value;
        self.set_register_8_value(Register8Bit::A, new_value);
        let zero = Some(new_value == 0);
        let substraction = Some(false);
        let half_carry = Some(false);
        let carry = Some(false);
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn or(&mut self, target: ArithmeticTarget) -> FlagsResults {
        let acc_value = self.get_register_8_value(Register8Bit::A);
        let value = match target {
            ArithmeticTarget::Register(register) => self.get_register_8_value(register),
            ArithmeticTarget::Value(v) => v,
        };
        let new_value = acc_value | value;
        self.set_register_8_value(Register8Bit::A, new_value);
        let zero = Some(new_value == 0);
        let substraction = Some(false);
        let half_carry = Some(false);
        let carry = Some(false);
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn compare(&mut self, target: ArithmeticTarget) -> FlagsResults {
        let acc_value = self.get_register_8_value(Register8Bit::A);
        let value = match target {
            ArithmeticTarget::Register(register) => self.get_register_8_value(register),
            ArithmeticTarget::Value(v) => v,
        };
        let (result, did_overflow) = acc_value.overflowing_sub(value);
        let zero = Some(result == 0);
        let substraction = Some(true);
        let half_carry = Some(Self::did_half_carry_sub_8(acc_value, value));
        let carry = Some(did_overflow);
        FlagsResults::new(zero, substraction, half_carry, carry)
    }

    fn return_subroutine(&mut self) -> FlagsResults {
        let stack_pointer = self.registers.get_16_bit_register(Register16Bit::SP);
        self.registers.increase_stack_pointer();
        self.registers.increase_stack_pointer();
        self.registers.set_program_counter(stack_pointer);
        FlagsResults::default()
    }

    fn conditional_return_subroutine(&mut self, condition: Condition) -> FlagsResults {
        if self.check_condition(condition) {
            self.return_subroutine()
        } else {
            FlagsResults::default()
        }
    }

    fn jump(&mut self, address: u16) -> FlagsResults {
        self.registers.set_program_counter(address);
        FlagsResults::default()
    }

    fn jump_conditional(&mut self, condition: Condition, address: u16) -> FlagsResults {
        if self.check_condition(condition) {
            self.jump(address)
        } else {
            FlagsResults::default()
        }
    }

    fn jump_hl(&mut self) -> FlagsResults {
        let address = self.registers.get_16_bit_register(Register16Bit::HL);
        self.jump(address)
    }

    fn return_subroutine_interrupt(&mut self) -> FlagsResults {
        self.mmu.enable_interrupt();
        self.return_subroutine()
    }

    fn call(&mut self, address: u16) -> FlagsResults {
        let program_counter = self.registers.get_program_counter();
        let msb_pc = (program_counter & 0xFF00) >> 8;
        let lsb_pc = program_counter & 0x00FF;

        self.registers.decrease_stack_pointer();
        self.mmu.write_byte(
            self.registers.get_16_bit_register(Register16Bit::SP),
            msb_pc as u8,
        );

        self.registers.decrease_stack_pointer();
        self.mmu.write_byte(
            self.registers.get_16_bit_register(Register16Bit::SP),
            lsb_pc as u8,
        );

        self.registers.set_program_counter(address);
        FlagsResults::default()
    }

    fn conditional_call(&mut self, condition: Condition, address: u16) -> FlagsResults {
        if self.check_condition(condition) {
            self.call(address)
        } else {
            FlagsResults::default()
        }
    }

    fn restart(&mut self, target: u8) -> FlagsResults {
        let address = target as u16;
        self.call(address)
    }

    fn pop_stack(&mut self, register: StackRegister) -> FlagsResults {
        let lsb = self
            .mmu
            .read_byte(self.registers.get_16_bit_register(Register16Bit::SP))
            as u16;
        self.registers.increase_stack_pointer();
        let msb = self
            .mmu
            .read_byte(self.registers.get_16_bit_register(Register16Bit::SP))
            as u16;
        self.registers.increase_stack_pointer();
        let read_value = (msb << 8) | lsb;
        match register {
            StackRegister::BC => self
                .registers
                .set_16_bit_register(Register16Bit::BC, read_value),
            StackRegister::DE => self
                .registers
                .set_16_bit_register(Register16Bit::DE, read_value),
            StackRegister::HL => self
                .registers
                .set_16_bit_register(Register16Bit::HL, read_value),
            StackRegister::AF => self
                .registers
                .set_16_bit_register(Register16Bit::AF, read_value),
        }
        FlagsResults::default()
    }

    fn push_stack(&mut self, register: StackRegister) -> FlagsResults {
        let value_read = match register {
            StackRegister::BC => self.registers.get_16_bit_register(Register16Bit::BC),
            StackRegister::DE => self.registers.get_16_bit_register(Register16Bit::DE),
            StackRegister::HL => self.registers.get_16_bit_register(Register16Bit::HL),
            StackRegister::AF => self.registers.get_16_bit_register(Register16Bit::AF),
        };

        let lsb = (value_read & 0x00FF) as u8;
        let msb = ((value_read & 0xFF00) >> 8) as u8;
        self.registers.decrease_stack_pointer();
        self.mmu
            .write_byte(self.registers.get_16_bit_register(Register16Bit::SP), msb);
        self.registers.decrease_stack_pointer();
        self.mmu
            .write_byte(self.registers.get_16_bit_register(Register16Bit::SP), lsb);
        FlagsResults::default()
    }

    fn load_to_mem_from_acc(&mut self, acc_memory_target: AccMemoryTarget) -> FlagsResults {
        let address = match acc_memory_target {
            AccMemoryTarget::RegC => {
                (0xFF as u16) | (self.get_register_8_value(Register8Bit::C) as u16)
            }
            AccMemoryTarget::Imm8(lsb) => (0xFF as u16) | (lsb as u16),
            AccMemoryTarget::Imm16(address) => address,
        };
        self.mmu
            .write_byte(address, self.get_register_8_value(Register8Bit::A));
        FlagsResults::default()
    }

    fn load_to_acc_from_mem(&mut self, acc_memory_target: AccMemoryTarget) -> FlagsResults {
        let address = match acc_memory_target {
            AccMemoryTarget::RegC => {
                (0xFF as u16) | (self.get_register_8_value(Register8Bit::C) as u16)
            }
            AccMemoryTarget::Imm8(lsb) => (0xFF as u16) | (lsb as u16),
            AccMemoryTarget::Imm16(address) => address,
        };
        let data_read = self.mmu.read_byte(address);
        self.set_register_8_value(Register8Bit::A, data_read);
        FlagsResults::default()
    }

    fn check_condition(&self, condition: Condition) -> bool {
        match condition {
            Condition::NonZero => !self.registers.get_zero(),
            Condition::Zero => self.registers.get_zero(),
            Condition::NonCarry => !self.registers.get_carry_flag(),
            Condition::Carry => self.registers.get_carry_flag(),
        }
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

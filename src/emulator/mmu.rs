const ROM_BANK_SIZE: usize = 0x4000;
const VRAM_SIZE: usize = 0x2000;
const EXTERNAL_RAM_SIZE: usize = 0x2000;
const WORK_RAM_SIZE: usize = 0x2000;
const ECHO_RAM_SIZE: usize = 0x2000 - 0x0200;
const OAM_SIZE: usize = 0xA0;
const UNUSED_MEMORY_SIZE: usize = 0x100 - 0xA0;
const IO_REGISTERS_SIZE: usize = 0x80;
const HIGH_RAM_SIZE: usize = 0x7F;

enum MemorySection {
    FixedRomBank,
    SwitchRomBank,
    VRAM,
    ExternalRam,
    WorkRam,
    EchoRam,
    OAM,
    Unused,
    IORegisters,
    HighRam,
    InterruptRegister,
}

impl From<u16> for MemorySection {
    fn from(address: u16) -> Self {
        match address {
            0x0000..0x4000 => Self::FixedRomBank,
            0x4000..0x8000 => Self::SwitchRomBank,
            0x8000..0xA000 => Self::VRAM,
            0xA000..0xC000 => Self::ExternalRam,
            0xC000..0xE000 => Self::WorkRam,
            0xE000..0xFE00 => Self::EchoRam,
            0xFE00..0xFEA0 => Self::OAM,
            0xFEA0..0xFF00 => Self::Unused,
            0xFF00..0xFF80 => Self::IORegisters,
            0xFF80..0xFFFF => Self::HighRam,
            0xFFFF => Self::InterruptRegister,
        }
    }
}

impl MemorySection {
    fn get_beginning_address(&self) -> u16 {
        match self {
            Self::FixedRomBank => 0x0000,
            Self::SwitchRomBank => 0x4000,
            Self::VRAM => 0x8000,
            Self::ExternalRam => 0xA000,
            Self::WorkRam => 0xC000,
            Self::EchoRam => 0xE000,
            Self::OAM => 0xFE00,
            Self::Unused => 0xFEA0,
            Self::IORegisters => 0xFF00,
            Self::HighRam => 0xFF80,
            Self::InterruptRegister => 0xFFFF,
        }
    }
}

pub struct MMU {
    fixed_rom_bank: [u8; ROM_BANK_SIZE],
    switchable_rom_bank: [u8; ROM_BANK_SIZE],
    video_ram: [u8; VRAM_SIZE],
    external_ram: [u8; EXTERNAL_RAM_SIZE],
    work_ram: [u8; WORK_RAM_SIZE],
    echo_ram: [u8; ECHO_RAM_SIZE],
    oam: [u8; OAM_SIZE],
    unused_memory: [u8; UNUSED_MEMORY_SIZE],
    io_registers: [u8; IO_REGISTERS_SIZE],
    high_ram: [u8; HIGH_RAM_SIZE],
    is_interrupt_enabled: bool,
}

impl MMU {
    pub fn read_byte(&self, address: u16) -> u8 {
        let memory_section = MemorySection::from(address);
        let start_address = memory_section.get_beginning_address();
        let address_in_section = (address - start_address) as usize;
        match memory_section {
            MemorySection::FixedRomBank => self.fixed_rom_bank[address_in_section],
            MemorySection::SwitchRomBank => self.switchable_rom_bank[address_in_section],
            MemorySection::VRAM => self.video_ram[address_in_section],
            MemorySection::ExternalRam => self.external_ram[address_in_section],
            MemorySection::WorkRam => self.work_ram[address_in_section],
            MemorySection::EchoRam => self.echo_ram[address_in_section],
            MemorySection::OAM => self.oam[address_in_section],
            MemorySection::Unused => 0,
            MemorySection::IORegisters => self.io_registers[address_in_section],
            MemorySection::HighRam => self.high_ram[address_in_section],
            MemorySection::InterruptRegister => self.is_interrupt_enabled as u8,
        }
    }

    pub fn write_byte(&mut self, address: u16, value: u8) {
        let memory_section = MemorySection::from(address);
        let start_address = memory_section.get_beginning_address();
        let address_in_section = (address - start_address) as usize;
        match memory_section {
            MemorySection::FixedRomBank => (),
            MemorySection::SwitchRomBank => (),
            MemorySection::VRAM => self.video_ram[address_in_section] = value,
            MemorySection::ExternalRam => self.external_ram[address_in_section] = value,
            MemorySection::WorkRam => self.work_ram[address_in_section] = value,
            MemorySection::EchoRam => self.echo_ram[address_in_section] = value,
            MemorySection::OAM => self.oam[address_in_section] = value,
            MemorySection::Unused => (),
            MemorySection::IORegisters => self.io_registers[address_in_section] = value,
            MemorySection::HighRam => self.high_ram[address_in_section] = value,
            MemorySection::InterruptRegister => self.is_interrupt_enabled = value == 0,
        };
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_total_memory_size() {
        let total_memory_size = ROM_BANK_SIZE
            + ROM_BANK_SIZE
            + VRAM_SIZE
            + EXTERNAL_RAM_SIZE
            + WORK_RAM_SIZE
            + ECHO_RAM_SIZE
            + OAM_SIZE
            + UNUSED_MEMORY_SIZE
            + IO_REGISTERS_SIZE
            + HIGH_RAM_SIZE
            + 1;
        assert_eq!(total_memory_size, 0x10000);
    }
}

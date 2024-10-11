use std::usize;

pub struct Memory {
    memory: [u8; 0xFFFF],
}

impl Memory {
    pub fn get_memory_value(&self, address: u16) -> u8 {
        self.memory[address as usize]
    }

    pub fn set_memory_value(&mut self, address: u16, new_value: u8) {
        self.memory[address as usize] = new_value;
    }
}

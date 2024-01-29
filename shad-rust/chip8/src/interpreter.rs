use crate::{
    data::{Address, Nibble, OpCode, RegisterIndex, Word},
    error::{Error, Result},
    image::Image,
    platform::{Platform, Point, Sprite},
    Key, Offset,
};

////////////////////////////////////////////////////////////////////////////////

pub const SCREEN_WIDTH: usize = 64;
pub const SCREEN_HEIGHT: usize = 32;
pub const KEYBOARD_SIZE: usize = 16;
pub const REGISTERS_AMOUNT: usize = 16;
pub const CALLSTACK_SIZE: usize = 16;

////////////////////////////////////////////////////////////////////////////////
#[derive(Default)]
pub struct Registers([Word; REGISTERS_AMOUNT]);

impl Registers {
    pub fn set(&mut self, index: RegisterIndex, word: Word) {
        self.0[index.as_usize()] = word;
    }
    pub fn set_f(&mut self, value: bool) {
        self.0[15] = u8::from(value);
    }
    pub fn get(&self, index: RegisterIndex) -> Word {
        self.0[index.as_usize()]
    }
    pub fn at(&self, index: usize) -> Word {
        self.0[index]
    }
    pub fn get_key(&self, index: RegisterIndex) -> Key {
        Nibble::try_from(self.get(index)).unwrap()
    }
    pub fn add(&mut self, index: RegisterIndex, value: Word) {
        let sum = self.0[index.as_usize()].overflowing_add(value);
        self.0[index.as_usize()] = sum.0;
    }
    pub fn add_with_carry(&mut self, x: RegisterIndex, y: RegisterIndex) {
        let (sum, carry) = self.0[x.as_usize()].overflowing_add(self.0[y.as_usize()]);
        self.set(x, sum);
        self.set_f(carry);
    }
    pub fn sub_with_carry(&mut self, x: RegisterIndex, y: RegisterIndex, reversed: bool) {
        let value_x = x.as_usize();
        let value_y = y.as_usize();
        let (sub, carry) = if reversed {
            self.0[value_y].overflowing_sub(self.0[value_x])
        } else {
            self.0[value_x].overflowing_sub(self.0[value_y])
        };
        self.set(x, sub);
        self.set_f(!carry);
    }
    pub fn shift(&mut self, x: RegisterIndex, y: RegisterIndex, right: bool) {
        let value = self.get(y);
        let (shifted_value, shifted_out) = if right {
            (value >> 1, value & 0b00000001 != 0)
        } else {
            (value << 1, value & 0b10000000 != 0)
        };
        self.set(x, shifted_value);
        self.set_f(shifted_out);
    }
    pub fn set_slice(&mut self, data: &[u8]) {
        for (i, byte) in data.iter().take(self.0.len()).enumerate() {
            self.0[i] = *byte;
        }
    }
    pub fn get_slice(&mut self, last: RegisterIndex) -> &[u8] {
        &self.0[0..last.as_usize() + 1]
    }
}

#[derive(Default)]
pub struct CallStack {
    stack: [Address; CALLSTACK_SIZE],
    pointer: usize,
}

impl CallStack {
    fn push(&mut self, address: Address) -> Result<()> {
        if self.pointer + 1 == CALLSTACK_SIZE {
            return Err(Error::StackOverflow);
        }
        self.pointer += 1;
        self.stack[self.pointer] = address;
        Ok(())
    }
    fn pop(&mut self) -> Result<Address> {
        if self.pointer == 0 {
            return Err(Error::StackUnderflow);
        }
        self.pointer -= 1;
        Ok(self.stack[self.pointer + 1])
    }
}

pub struct Memory {
    bytes: [u8; Address::DOMAIN_SIZE],
    pc: Address,
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            bytes: [0; Address::DOMAIN_SIZE],
            pc: Address::new(0x200),
        }
    }
}

impl Memory {
    fn get_next_op_code(&mut self) -> OpCode {
        let current = self.pc.as_usize();
        self.pc += 2;
        OpCode::from_bytes(self.bytes[current], self.bytes[current + 1])
    }
    fn get_slice(&self, start: Address, size: usize) -> &[u8] {
        &self.bytes[start.as_usize()..start.as_usize() + size]
    }
    fn set_slice(&mut self, start: Address, data: &[u8]) {
        for (i, byte) in data.iter().enumerate() {
            self.bytes[start.as_usize() + i] = *byte;
        }
    }
    fn set(&mut self, index: Address, value: u8) {
        self.bytes[index.as_usize()] = value;
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct Interpreter<P: Platform> {
    platform: P,
    v: Registers,
    i: Address,
    stack: CallStack,
    memory: Memory,
}

impl<P: Platform> Interpreter<P> {
    pub fn new(image: impl Image, platform: P) -> Self {
        let mut memory = Memory::default();
        image.load_into_memory(&mut memory.bytes);
        Self {
            platform,
            v: Default::default(),
            i: Default::default(),
            stack: Default::default(),
            memory,
        }
    }
    pub fn platform(&self) -> &P {
        &self.platform
    }
    pub fn platform_mut(&mut self) -> &mut P {
        &mut self.platform
    }
    pub fn run_next_instruction(&mut self) -> Result<()> {
        let next_operation = Operation::try_from(self.memory.get_next_op_code())?;
        match next_operation {
            Operation::ClearScreen => self.platform.clear_screen(),
            Operation::Return => self.memory.pc = self.stack.pop()?,
            Operation::Jump(address) => self.memory.pc = address,
            Operation::Call(address) => {
                self.stack.push(self.memory.pc)?;
                self.memory.pc = address;
            }
            Operation::SkipIfEqual(x, word) => {
                if self.v.get(x) == word {
                    self.memory.pc += 2;
                }
            }
            Operation::SkipIfNotEqual(x, word) => {
                if self.v.get(x) != word {
                    self.memory.pc += 2;
                }
            }
            Operation::SkipIfRegistersEqual(x, y) => {
                if self.v.get(x) == self.v.get(y) {
                    self.memory.pc += 2;
                }
            }
            Operation::SetRegister(x, word) => self.v.set(x, word),
            Operation::AddValue(x, word) => self.v.add(x, word),
            Operation::SetToRegister(x, y) => self.v.set(x, self.v.get(y)),
            Operation::Or(x, y) => {
                let word = self.v.get(x) | self.v.get(y);
                self.v.set(x, word);
                self.v.set_f(false);
            }
            Operation::And(x, y) => {
                let word = self.v.get(x) & self.v.get(y);
                self.v.set(x, word);
                self.v.set_f(false);
            }
            Operation::Xor(x, y) => {
                let word = self.v.get(x) ^ self.v.get(y);
                self.v.set(x, word);
                self.v.set_f(false);
            }
            Operation::AddRegister(x, y) => self.v.add_with_carry(x, y),
            Operation::SubRegister(x, y) => self.v.sub_with_carry(x, y, false),
            Operation::ShiftRight(x, y) => self.v.shift(x, y, true),
            Operation::SubRegisterReversed(x, y) => self.v.sub_with_carry(x, y, true),
            Operation::ShiftLeft(x, y) => self.v.shift(x, y, false),
            Operation::SkipIfRegistersNotEqual(x, y) => {
                if self.v.get(x) != self.v.get(y) {
                    self.memory.pc += 2;
                }
            }
            Operation::SetIndexRegister(address) => self.i = address,
            Operation::JumpV0(address) => self.memory.pc = address + (self.v.at(0) as Offset),
            Operation::SetToRandom(_, _) => todo!(),
            Operation::Draw(x, y, n) => {
                let point = Point {
                    x: self.v.get(x),
                    y: self.v.get(y),
                };
                let sprite = Sprite::new(self.memory.get_slice(self.i, n.as_usize()));
                let collision = self.platform.draw_sprite(point, sprite);
                self.v.set_f(collision);
            }
            Operation::SkipIfKeyDown(x) => {
                self.platform.consume_key_press();
                if self.platform.is_key_down(self.v.get_key(x)) {
                    self.memory.pc += 2;
                }
            }
            Operation::SkipIfKeyUp(x) => {
                self.platform.consume_key_press();
                if !self.platform.is_key_down(self.v.get_key(x)) {
                    self.memory.pc += 2;
                }
            }
            Operation::GetDelayTimer(x) => self.v.set(x, self.platform.get_delay_timer()),
            Operation::WaitForKey(x) => {
                if let Some(last) = self.platform.consume_key_press() {
                    self.v.set(x, last.as_u8());
                } else {
                    self.memory.pc += -2;
                }
            }
            Operation::SetDelayTimer(x) => self.platform.set_delay_timer(self.v.get(x)),
            Operation::SetSoundTimer(x) => self.platform.set_sound_timer(self.v.get(x)),
            Operation::IncrementIndexRegister(x) => self.i += self.v.get(x) as Offset,
            Operation::SetIndexRegisterToSprite(_) => todo!(),
            Operation::ToDecimal(x) => {
                let val = self.v.get(x);
                self.memory.set(self.i, val / 100);
                self.memory.set(self.i + (1 as Offset), (val / 10) % 10);
                self.memory.set(self.i + (2 as Offset), val % 10);
            }
            Operation::WriteMemory(x) => {
                let registers = self.v.get_slice(x);
                self.memory.set_slice(self.i, registers);
                self.i += x.as_offset();
                self.i += 1 as Offset;
            }
            Operation::ReadMemory(x) => {
                let data = self.memory.get_slice(self.i, x.as_usize() + 1);
                self.v.set_slice(data);
                self.i += x.as_offset();
                self.i += 1 as Offset;
            }
        }
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, Clone, Copy)]
pub enum Operation {
    ClearScreen,
    Return,
    Jump(Address),
    Call(Address),
    SkipIfEqual(RegisterIndex, Word),
    SkipIfNotEqual(RegisterIndex, Word),
    SkipIfRegistersEqual(RegisterIndex, RegisterIndex),
    SetRegister(RegisterIndex, Word),
    AddValue(RegisterIndex, Word),
    SetToRegister(RegisterIndex, RegisterIndex),
    Or(RegisterIndex, RegisterIndex),
    And(RegisterIndex, RegisterIndex),
    Xor(RegisterIndex, RegisterIndex),
    AddRegister(RegisterIndex, RegisterIndex),
    SubRegister(RegisterIndex, RegisterIndex),
    ShiftRight(RegisterIndex, RegisterIndex),
    SubRegisterReversed(RegisterIndex, RegisterIndex),
    ShiftLeft(RegisterIndex, RegisterIndex),
    SkipIfRegistersNotEqual(RegisterIndex, RegisterIndex),
    SetIndexRegister(Address),
    JumpV0(Address),
    SetToRandom(RegisterIndex, Word),
    Draw(RegisterIndex, RegisterIndex, Nibble),
    SkipIfKeyDown(RegisterIndex),
    SkipIfKeyUp(RegisterIndex),
    GetDelayTimer(RegisterIndex),
    WaitForKey(RegisterIndex),
    SetDelayTimer(RegisterIndex),
    SetSoundTimer(RegisterIndex),
    IncrementIndexRegister(RegisterIndex),
    SetIndexRegisterToSprite(Nibble),
    ToDecimal(RegisterIndex),
    WriteMemory(Nibble),
    ReadMemory(Nibble),
}

impl TryFrom<OpCode> for Operation {
    type Error = Error;

    fn try_from(code: OpCode) -> Result<Self> {
        let unknown_opcode_error = Err(Error::UnknownOpCode(code));
        let op = match code.as_u16() {
            0x00e0 => Self::ClearScreen,
            0x00ee => Self::Return,
            _ => {
                let address = code.extract_address();
                let nibbles = code.extract_nibbles();
                let word1 = code.extract_word(1);
                match nibbles[0].as_u8() {
                    0xa => Self::SetIndexRegister(address),
                    0x1 => Self::Jump(address),
                    0x2 => Self::Call(address),
                    0x3 => Self::SkipIfEqual(nibbles[1], word1),
                    0x4 => Self::SkipIfNotEqual(nibbles[1], word1),
                    0x5 => Self::SkipIfRegistersEqual(nibbles[1], nibbles[2]),
                    0x6 => Self::SetRegister(nibbles[1], word1),
                    0x7 => Self::AddValue(nibbles[1], word1),
                    0x8 => match nibbles[3].as_u8() {
                        0x0 => Self::SetToRegister(nibbles[1], nibbles[2]),
                        0x1 => Self::Or(nibbles[1], nibbles[2]),
                        0x2 => Self::And(nibbles[1], nibbles[2]),
                        0x3 => Self::Xor(nibbles[1], nibbles[2]),
                        0x4 => Self::AddRegister(nibbles[1], nibbles[2]),
                        0x5 => Self::SubRegister(nibbles[1], nibbles[2]),
                        0x6 => Self::ShiftRight(nibbles[1], nibbles[2]),
                        0x7 => Self::SubRegisterReversed(nibbles[1], nibbles[2]),
                        0xe => Self::ShiftLeft(nibbles[1], nibbles[2]),
                        _ => return unknown_opcode_error,
                    },
                    0x9 => Self::SkipIfRegistersNotEqual(nibbles[1], nibbles[2]),
                    0xb => Self::JumpV0(address),
                    0xd => Self::Draw(nibbles[1], nibbles[2], nibbles[3]),
                    0xe => match word1 {
                        0x9e => Self::SkipIfKeyDown(nibbles[1]),
                        0xa1 => Self::SkipIfKeyUp(nibbles[1]),
                        _ => return unknown_opcode_error,
                    },
                    0xf => match word1 {
                        0x07 => Self::GetDelayTimer(nibbles[1]),
                        0x0a => Self::WaitForKey(nibbles[1]),
                        0x15 => Self::SetDelayTimer(nibbles[1]),
                        0x18 => Self::SetSoundTimer(nibbles[1]),
                        0x1e => Self::IncrementIndexRegister(nibbles[1]),
                        0x33 => Self::ToDecimal(nibbles[1]),
                        0x55 => Self::WriteMemory(nibbles[1]),
                        0x65 => Self::ReadMemory(nibbles[1]),
                        _ => return unknown_opcode_error,
                    },
                    _ => return unknown_opcode_error,
                }
            }
        };
        Ok(op)
    }
}

////////////////////////////////////////////////////////////////////////////////

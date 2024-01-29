use crate::{
    data::Word,
    error::{Error, Result},
    image::Image,
    interpreter::{Interpreter, KEYBOARD_SIZE, SCREEN_HEIGHT, SCREEN_WIDTH},
    platform::{Key, Platform, Point, Sprite},
    KeyEventKind,
};

use core::time::Duration;

////////////////////////////////////////////////////////////////////////////////

pub struct FrameBuffer([[bool; SCREEN_WIDTH]; SCREEN_HEIGHT]);

impl Default for FrameBuffer {
    fn default() -> Self {
        Self([[false; SCREEN_WIDTH]; SCREEN_HEIGHT])
    }
}

impl FrameBuffer {
    pub fn mask(&mut self, point: Point, start: Point) -> bool {
        let loc = start + point;
        let x = loc.x as usize;
        let y = loc.y as usize;
        if x >= SCREEN_WIDTH || y >= SCREEN_HEIGHT {
            return false;
        }
        let prev = self.0[y][x];
        self.0[y][x] = !prev;
        prev
    }
    pub fn iter_rows(&self) -> impl Iterator<Item = &[bool; SCREEN_WIDTH]> {
        self.0.iter()
    }

    pub fn reset(&mut self) {
        self.0
            .iter_mut()
            .for_each(|row| row.iter_mut().for_each(|element| *element = false));
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct ManagedKeyBoard {
    keys: [KeyEventKind; KEYBOARD_SIZE],
    last_pressed: Option<Key>,
}

impl Default for ManagedKeyBoard {
    fn default() -> Self {
        Self {
            keys: [KeyEventKind::Released; KEYBOARD_SIZE],
            last_pressed: None,
        }
    }
}

impl ManagedKeyBoard {
    fn is_key_valid(&self, key: Key) -> bool {
        key.as_usize() < self.keys.len()
    }
    pub fn is_key_down(&self, key: Key) -> Result<bool> {
        if self.is_key_valid(key) {
            Ok(self.keys[key.as_usize()] == KeyEventKind::Pressed)
        } else {
            Err(Error::InvalidKey(key.as_u8()))
        }
    }

    pub fn set_key_down(&mut self, key: Key, is_down: bool) -> Result<()> {
        if self.is_key_valid(key) {
            if is_down {
                self.last_pressed = Some(key);
                self.keys[key.as_usize()] = KeyEventKind::Pressed;
            } else {
                self.keys[key.as_usize()] = KeyEventKind::Released;
            }
            Ok(())
        } else {
            Err(Error::InvalidKey(key.as_u8()))
        }
    }

    pub fn consume_key_press(&mut self) -> Option<Key> {
        if let Some(last) = self.last_pressed {
            if !self.is_key_down(last).unwrap() {
                self.last_pressed = None;
                return Some(last);
            }
        }
        None
    }
}

////////////////////////////////////////////////////////////////////////////////

pub trait RandomNumberGenerator: FnMut() -> Word {}

impl<R: FnMut() -> Word> RandomNumberGenerator for R {}

////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
struct ManagedPlatform<R: RandomNumberGenerator> {
    rand: R,
    frame_buffer: FrameBuffer,
    delay_timer: Word,
    sound_timer: Word,
    keyboard: ManagedKeyBoard,
}

impl<R: RandomNumberGenerator> Platform for ManagedPlatform<R> {
    fn draw_sprite(&mut self, pos: Point, sprite: Sprite) -> bool {
        let pos_fixed = pos.fit(SCREEN_WIDTH, SCREEN_HEIGHT);
        let mut collision = false;
        sprite.iter_pixels().for_each(|point| {
            collision |= self.frame_buffer.mask(point, pos_fixed);
        });
        collision
    }
    fn clear_screen(&mut self) {
        self.frame_buffer.reset()
    }
    fn get_delay_timer(&self) -> Word {
        self.delay_timer
    }
    fn get_sound_timer(&self) -> Word {
        self.sound_timer
    }
    fn set_delay_timer(&mut self, value: Word) {
        self.delay_timer = value;
    }
    fn set_sound_timer(&mut self, value: Word) {
        self.sound_timer = value;
    }
    fn is_key_down(&self, key: Key) -> bool {
        self.keyboard.is_key_down(key).unwrap()
    }
    fn consume_key_press(&mut self) -> Option<Key> {
        self.keyboard.consume_key_press()
    }
    fn get_random_word(&mut self) -> Word {
        (self.rand)()
    }
}

impl<R: RandomNumberGenerator> ManagedPlatform<R> {
    fn new(rand: R) -> Self {
        Self {
            rand,
            frame_buffer: Default::default(),
            delay_timer: 0,
            sound_timer: 0,
            keyboard: Default::default(),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

pub struct ManagedInterpreter<R: RandomNumberGenerator> {
    inner: Interpreter<ManagedPlatform<R>>,
    operation_duration: Duration,
    delay_tick_duration: Duration,
    sound_tick_duration: Duration,
}

impl<R: RandomNumberGenerator> ManagedInterpreter<R> {
    pub const DEFAULT_OPERATION_DURATION: Duration = Duration::from_millis(2);
    pub const DEFAULT_DELAY_TICK_DURATION: Duration = Duration::from_nanos(16666667);
    pub const DEFAULT_SOUND_TICK_DURATION: Duration = Duration::from_nanos(16666667);

    pub fn new(image: impl Image, rand: R) -> Self {
        Self::new_with_durations(
            image,
            rand,
            Self::DEFAULT_OPERATION_DURATION,
            Self::DEFAULT_DELAY_TICK_DURATION,
            Self::DEFAULT_SOUND_TICK_DURATION,
        )
    }

    pub fn new_with_durations(
        image: impl Image,
        rand: R,
        operation_duration: Duration,
        delay_tick_duration: Duration,
        sound_tick_duration: Duration,
    ) -> Self {
        Self {
            inner: Interpreter::new(image, ManagedPlatform::new(rand)),
            operation_duration,
            delay_tick_duration,
            sound_tick_duration,
        }
    }

    pub fn simulate_one_instruction(&mut self) -> Result<()> {
        self.inner.run_next_instruction()
    }

    pub fn simulate_duration(&mut self, duration: Duration) -> Result<()> {
        for i in 0..duration.as_micros() {
            if i % self.operation_duration.as_micros() == 0 {
                self.inner.run_next_instruction()?
            }
            if i % self.delay_tick_duration.as_micros() == 0 {
                let platform: &mut ManagedPlatform<R> = self.inner.platform_mut();
                platform.set_delay_timer(platform.get_delay_timer().saturating_sub(1));
            }
            if i % self.sound_tick_duration.as_micros() == 0 {
                let platform = self.inner.platform_mut();
                platform.set_sound_timer(platform.get_sound_timer().saturating_sub(1));
            }
        }
        Ok(())
    }

    pub fn frame_buffer(&self) -> &FrameBuffer {
        &self.inner.platform().frame_buffer
    }

    pub fn set_key_down(&mut self, key: Key, is_down: bool) {
        self.inner
            .platform_mut()
            .keyboard
            .set_key_down(key, is_down)
            .unwrap();
    }
}

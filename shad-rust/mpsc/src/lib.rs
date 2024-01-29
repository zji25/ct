#![forbid(unsafe_code)]

use std::{
    cell::RefCell,
    collections::VecDeque,
    fmt::Debug,
    rc::{Rc, Weak},
};

use thiserror::Error;

////////////////////////////////////////////////////////////////////////////////

#[derive(Error, Debug)]
#[error("channel is closed")]
pub struct SendError<T: Debug> {
    pub value: T,
}

pub type Buffer<T> = RefCell<VecDeque<T>>;

pub struct Sender<T> {
    buffer: Weak<Buffer<T>>,
}

impl<T> From<Weak<Buffer<T>>> for Sender<T> {
    fn from(buffer: Weak<Buffer<T>>) -> Self {
        Self { buffer }
    }
}

impl<T: Debug> Sender<T> {
    pub fn send(&self, value: T) -> Result<(), SendError<T>> {
        if let Some(rc) = self.buffer.upgrade() {
            rc.as_ref().borrow_mut().push_back(value);
            drop(rc);
            Ok(())
        } else {
            Err(SendError { value })
        }
    }

    pub fn is_closed(&self) -> bool {
        self.buffer.upgrade().is_none()
    }

    pub fn same_channel(&self, other: &Self) -> bool {
        self.buffer.ptr_eq(&other.buffer)
    }
}

impl<T> Clone for Sender<T> {
    fn clone(&self) -> Self {
        Self {
            buffer: self.buffer.clone(),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

#[derive(Error, Debug)]
pub enum ReceiveError {
    #[error("channel is empty")]
    Empty,
    #[error("channel is closed")]
    Closed,
}

pub struct Receiver<T> {
    buffer: Rc<Buffer<T>>,
    is_closed: bool,
}

impl<T> From<Rc<Buffer<T>>> for Receiver<T> {
    fn from(buffer: Rc<Buffer<T>>) -> Self {
        Self {
            buffer,
            is_closed: false,
        }
    }
}

impl<T> Receiver<T> {
    pub fn recv(&mut self) -> Result<T, ReceiveError> {
        if let Some(element) = self.buffer.as_ref().borrow_mut().pop_front() {
            return Ok(element);
        }
        if Rc::<RefCell<VecDeque<T>>>::weak_count(&self.buffer) == 0 {
            self.close();
        }
        if self.is_closed {
            return Err(ReceiveError::Closed);
        }
        Err(ReceiveError::Empty)
    }

    pub fn close(&mut self) {
        self.buffer = Rc::new(RefCell::from(self.buffer.take()));
        self.is_closed = true;
    }
}

impl<T> Drop for Receiver<T> {
    fn drop(&mut self) {
        self.close();
    }
}

////////////////////////////////////////////////////////////////////////////////

pub fn channel<T>() -> (Sender<T>, Receiver<T>) {
    let buffer = Rc::new(RefCell::new(VecDeque::<T>::default()));
    let weak = Rc::downgrade(&buffer);
    (Sender::from(weak), Receiver::from(buffer))
}

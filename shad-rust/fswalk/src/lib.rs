#![forbid(unsafe_code)]

use std::{
    fs::{self, ReadDir},
    io::{Error, ErrorKind::Unsupported, Result},
    path::Path,
};

////////////////////////////////////////////////////////////////////////////////

type Callback<'a> = dyn FnMut(&mut Handle) + 'a;

#[derive(Default)]
pub struct Walker<'a> {
    callbacks: Vec<Box<Callback<'a>>>,
}

impl<'a> Walker<'a> {
    pub fn new() -> Self {
        Self {
            callbacks: Vec::new(),
        }
    }

    pub fn add_callback<F>(&mut self, callback: F)
    where
        F: FnMut(&mut Handle) + 'a,
    {
        self.callbacks.push(Box::new(callback));
    }

    fn apply_callbacks(&mut self, handle: &mut Handle, last: usize) -> usize {
        let mut non_interested_indexes = Vec::new();
        for (i, callback) in self.callbacks.iter_mut().take(last).enumerate() {
            callback(handle);
            let interested = match handle {
                Handle::Dir(dir_handle) => dir_handle.did_descend(),
                Handle::File(file_handle) => file_handle.did_read(),
                _ => true,
            };
            if !interested {
                non_interested_indexes.push(i);
            }
        }
        let mut last_index = last;
        for &index in non_interested_indexes.iter().rev() {
            self.callbacks.swap(index, last_index - 1);
            last_index -= 1;
        }
        last_index
    }

    pub fn walk<P: AsRef<Path>>(&mut self, path: P) -> Result<()> {
        self.walk_until(path.as_ref(), self.callbacks.len())
    }

    fn walk_until(&mut self, path: &Path, last: usize) -> Result<()> {
        if last == 0 {
            return Ok(());
        }
        let mut handle = if path.is_file() {
            Handle::File(FileHandle::new(path))
        } else if path.is_dir() {
            Handle::Dir(DirHandle::new(path))
        } else {
            return Err(Error::from(Unsupported));
        };
        let new_last = self.apply_callbacks(&mut handle, last);
        match handle {
            Handle::File(file_handle) => self.process_file_handle(file_handle, new_last),
            Handle::Dir(dir_handle) => self.process_dir_handle(dir_handle, new_last),
            _ => panic!("should not go here"),
        }
    }

    fn process_dir_handle(&mut self, dir_handle: DirHandle, last: usize) -> Result<()> {
        match dir_handle.contents {
            Some(Ok(mut read_dir)) => read_dir.try_for_each(|wrapped_entry| match wrapped_entry {
                Ok(entry) => self.walk_until(entry.path().as_path(), last),
                Err(error) => Err(error),
            }),
            Some(Err(error)) => Err(error),
            None => Ok(()),
        }
    }

    fn process_file_handle(&mut self, file_handle: FileHandle, last: usize) -> Result<()> {
        match file_handle.contents {
            Some(Ok(content)) => {
                let mut content_handle = Handle::Content {
                    file_path: file_handle.path,
                    content: &content,
                };
                self.apply_callbacks(&mut content_handle, last);
                Ok(())
            }
            Some(Err(error)) => Err(error),
            None => Ok(()),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

pub enum Handle<'a> {
    Dir(DirHandle<'a>),
    File(FileHandle<'a>),
    Content {
        file_path: &'a Path,
        content: &'a [u8],
    },
}

pub struct DirHandle<'a> {
    path: &'a Path,
    did_descend: bool,
    contents: Option<Result<ReadDir>>,
}

impl<'a> DirHandle<'a> {
    pub fn new(path: &'a Path) -> Self {
        Self {
            path,
            did_descend: false,
            contents: None,
        }
    }
    pub fn descend(&mut self) {
        if self.contents.is_none() {
            self.contents = Some(fs::read_dir(self.path));
        }
        self.did_descend = true;
    }
    pub fn did_descend(&mut self) -> bool {
        let prev = self.did_descend;
        self.did_descend = false;
        prev
    }
    pub fn path(&self) -> &Path {
        self.path
    }
}

pub struct FileHandle<'a> {
    path: &'a Path,
    did_read: bool,
    contents: Option<Result<Vec<u8>>>,
}

impl<'a> FileHandle<'a> {
    pub fn new(path: &'a Path) -> Self {
        Self {
            path,
            did_read: false,
            contents: None,
        }
    }
    pub fn read(&mut self) {
        if self.contents.is_none() {
            self.contents = Some(fs::read(self.path));
        }
        self.did_read = true;
    }
    pub fn did_read(&mut self) -> bool {
        let prev = self.did_read;
        self.did_read = false;
        prev
    }
    pub fn path(&self) -> &Path {
        self.path
    }
}

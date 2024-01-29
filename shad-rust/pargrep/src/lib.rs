#![forbid(unsafe_code)]

use std::{
    fs::File,
    io::{self, BufRead, BufReader},
    path::{Path, PathBuf},
    sync::{
        mpsc::{channel, Sender},
        Arc,
    },
    thread,
};

////////////////////////////////////////////////////////////////////////////////

#[derive(Debug, PartialEq, Eq)]
pub struct Match {
    pub path: PathBuf,
    pub line: String,
    pub line_number: usize,
}

#[derive(Debug)]
pub struct Error {
    pub path: PathBuf,
    pub error: io::Error,
}

pub enum Event {
    Match(Match),
    Error(Error),
}

pub fn run<P: AsRef<Path>>(path: P, pattern: &str) -> Vec<Event> {
    let (sender, receiver) = channel::<Vec<Event>>();
    let arc_pattern = Arc::new(pattern.to_owned());
    walk(Arc::new(path.as_ref().to_owned()), &arc_pattern, &sender);
    drop(sender);
    receiver.into_iter().flatten().collect()
}

fn walk(path: Arc<PathBuf>, pattern: &Arc<String>, sender: &Sender<Vec<Event>>) {
    if path.is_dir() {
        match path.read_dir() {
            Ok(read_dir) => {
                read_dir.for_each(|entry| match entry {
                    Ok(direntry) => {
                        walk(Arc::new(direntry.path()), pattern, sender);
                    }
                    Err(_) => todo!(),
                });
            }
            Err(_) => todo!(),
        }
        return;
    }
    let new_sender = sender.clone();
    let new_pattern = pattern.clone();
    let new_path = path.clone();
    thread::spawn(move || {
        let _ = new_sender.send(process_file(new_path, new_pattern));
    });
}

fn process_file(path_buf: Arc<PathBuf>, pattern: Arc<String>) -> Vec<Event> {
    match File::open(path_buf.as_path()) {
        Ok(file) => {
            match BufReader::new(file)
                .lines()
                .collect::<Result<Vec<String>, io::Error>>()
            {
                Ok(lines) => lines
                    .into_iter()
                    .enumerate()
                    .filter_map(|(i, line)| {
                        if line.contains(pattern.as_str()) {
                            return Some(Event::Match(Match {
                                path: path_buf.to_path_buf(),
                                line,
                                line_number: i + 1,
                            }));
                        }
                        None
                    })
                    .collect(),
                Err(error) => vec![Event::Error(Error {
                    path: path_buf.to_path_buf(),
                    error,
                })],
            }
        }
        Err(error) => vec![Event::Error(Error {
            path: path_buf.to_path_buf(),
            error,
        })],
    }
}

#![forbid(unsafe_code)]

use std::net::{TcpListener, TcpStream};
use std::{io::copy, thread};

pub fn run_proxy(port: u32, destination: String) {
    let listener = TcpListener::bind(format!("127.0.0.1:{}", port)).unwrap();
    for incoming in listener.incoming() {
        let mut client = incoming.unwrap();
        let mut server = TcpStream::connect(&destination).unwrap();
        let mut client_clone = client.try_clone().unwrap();
        let mut server_clone = server.try_clone().unwrap();
        thread::spawn(move || copy(&mut client, &mut server).unwrap());
        thread::spawn(move || copy(&mut server_clone, &mut client_clone).unwrap());
    }
}

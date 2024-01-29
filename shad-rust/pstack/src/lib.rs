#![forbid(unsafe_code)]

use std::rc::Rc;

////////////////////////////////////////////////////////////////////////////////

pub struct PRef<T> {
    value: Rc<Node<T>>,
}

impl<T> std::ops::Deref for PRef<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value.element
    }
}

////////////////////////////////////////////////////////////////////////////////

struct Node<T> {
    element: T,
    next: Option<Rc<Node<T>>>,
}

////////////////////////////////////////////////////////////////////////////////

pub struct PStack<T> {
    head: Option<Rc<Node<T>>>,
    size: usize,
}

impl<T> Default for PStack<T> {
    fn default() -> Self {
        Self {
            head: None,
            size: 0,
        }
    }
}

impl<T> Clone for PStack<T> {
    fn clone(&self) -> Self {
        PStack {
            head: self.head.clone(),
            size: self.size,
        }
    }
}

impl<T> PStack<T> {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn push(&self, value: T) -> Self {
        PStack {
            head: Some(Rc::new(Node {
                element: value,
                next: self.head.clone(),
            })),
            size: self.size + 1,
        }
    }
    pub fn pop(&self) -> Option<(PRef<T>, Self)> {
        self.head.as_ref().map(|current_node| {
            (
                PRef {
                    value: Rc::clone(current_node),
                },
                PStack {
                    head: current_node.next.clone(),
                    size: self.size - 1,
                },
            )
        })
    }
    pub fn len(&self) -> usize {
        self.size
    }
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }
    pub fn iter(&self) -> impl Iterator<Item = PRef<T>> {
        PStackIterator {
            current: self.head.clone(),
        }
    }
}

////////////////////////////////////////////////////////////////////////////////

struct PStackIterator<T> {
    current: Option<Rc<Node<T>>>,
}

impl<T> Iterator for PStackIterator<T> {
    type Item = PRef<T>;
    fn next(&mut self) -> Option<Self::Item> {
        match self.current.clone() {
            Some(current_node) => {
                self.current = current_node.next.clone();
                Some(PRef {
                    value: Rc::clone(&current_node),
                })
            }
            _ => None,
        }
    }
}

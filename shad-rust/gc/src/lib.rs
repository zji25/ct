#![forbid(unsafe_code)]

pub use gc_derive::Scan;

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    marker::PhantomData,
    ops::Deref,
    rc::{Rc, Weak},
};

////////////////////////////////////////////////////////////////////////////////

pub struct Gc<T> {
    weak: Weak<T>,
}
impl<T> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Self {
            weak: self.weak.clone(),
        }
    }
}
impl<T> Gc<T> {
    pub fn borrow(&self) -> GcRef<'_, T> {
        GcRef {
            rc: self.weak.upgrade().unwrap(),
            lifetime: PhantomData,
        }
    }
}

pub struct GcRef<'a, T> {
    rc: Rc<T>,
    lifetime: PhantomData<&'a Gc<T>>,
}
impl<'a, T> Deref for GcRef<'a, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.rc
    }
}

////////////////////////////////////////////////////////////////////////////////

pub trait Scan {
    fn get_gcs(&self) -> Vec<usize> {
        vec![]
    }
}
impl<T> Scan for Gc<T> {
    fn get_gcs(&self) -> Vec<usize> {
        vec![self.weak.as_ptr() as usize]
    }
}
impl<T: Scan> Scan for Option<T> {
    fn get_gcs(&self) -> Vec<usize> {
        if let Some(x) = self {
            return x.get_gcs();
        }
        vec![]
    }
}
impl<T: Scan> Scan for Vec<T> {
    fn get_gcs(&self) -> Vec<usize> {
        self.iter().flat_map(Scan::get_gcs).collect()
    }
}
impl<T: Scan> Scan for RefCell<T> {
    fn get_gcs(&self) -> Vec<usize> {
        self.borrow().get_gcs()
    }
}
impl Scan for i32 {}

////////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
pub struct Arena {
    rcs: Vec<Rc<dyn Scan + 'static>>,
}

impl Arena {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn allocation_count(&self) -> usize {
        self.rcs.len()
    }
    pub fn alloc<T: Scan + 'static>(&mut self, obj: T) -> Gc<T> {
        let rc = Rc::new(obj);
        let gc = Gc {
            weak: Rc::downgrade(&rc),
        };
        self.rcs.push(rc);
        gc
    }
    pub fn sweep(&mut self) {
        let pointer_to_index = self
            .rcs
            .iter()
            .enumerate()
            .map(|(index, rc)| (Rc::as_ptr(rc) as *const () as usize, index))
            .collect::<HashMap<usize, usize>>();
        let mut count_inside = vec![0; self.allocation_count()];
        self.rcs.iter().for_each(|rc| {
            rc.get_gcs().iter().for_each(|l| {
                count_inside[pointer_to_index[l]] += 1;
            })
        });
        let mut marked = HashSet::<usize>::new();
        self.rcs.iter().enumerate().for_each(|(i, rc)| {
            if Rc::weak_count(rc) > count_inside[i] {
                self.mark_all(i, &mut marked, &pointer_to_index);
            }
        });
        let mut index = 0;
        self.rcs.retain(|_x| {
            let keep = marked.contains(&index);
            index += 1;
            keep
        });
    }

    fn mark_all(
        &self,
        i: usize,
        marked: &mut HashSet<usize>,
        pointer_to_ind: &HashMap<usize, usize>,
    ) {
        if marked.insert(i) {
            self.rcs[i]
                .get_gcs()
                .iter()
                .for_each(|x| match pointer_to_ind.get(x) {
                    Some(vv) => {
                        self.mark_all(*vv, marked, pointer_to_ind);
                    }
                    None => panic!(),
                });
        }
    }
}

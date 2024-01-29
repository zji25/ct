#![forbid(unsafe_code)]

#[macro_export]
macro_rules! deque {
    () => (::std::collections::VecDeque::new());
    ($x:expr; $n:expr) => (
        ::std::iter::repeat($x).take($n).collect::<::std::collections::VecDeque<_>>()
    );
    ($($x:expr),+ $(,)?) => (
        [$($x),*].into_iter().collect::<::std::collections::VecDeque<_>>()
    );
}

#[macro_export]
macro_rules! sorted_vec {
    () => (::std::vec::Vec::new());
    ($x:expr; $n:expr) => (::std:vec::from_elem($x, $n));
    ($($x:expr),+ $(,)?) => ({
        let mut vec = [$($x),*].into_iter().collect::<::std::vec::Vec<_>>();
        vec.sort();
        vec
    });
}

#[macro_export]
macro_rules! map {
    ($($x:expr=>$y:expr),* $(,)?) => (
        ::std::collections::HashMap::<_, _>::from([$(($x,$y)),*])
    )
}

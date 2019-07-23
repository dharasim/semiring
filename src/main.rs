fn main() {
    use crate::semiring::Semiring;
    let from_string = semiring::free::FreeSemiring::from;
    let a = from_string("A");
    let b = from_string("B");
    let f = | s | match s {
        "A" => 1,
        "B" => 2,
        _   => 3,
    };
    println!("{}", a.mul(&b).eval(&f));
}

mod semiring {
    pub trait Semiring {
        fn add(&self, other: &Self) -> Self;
        fn mul(&self, other: &Self) -> Self;
        fn zero() -> Self;
        fn one() -> Self;
    }

    impl Semiring for i32 {
        fn add(&self, other: &Self) -> Self {
            self + other
        }
        fn mul(&self, other: &Self) -> Self {
            self * other
        }
        fn zero() -> Self {
            0
        }
        fn one() -> Self {
            1
        }
    }

    pub mod free {
        use super::Semiring;
        use std::ops::Deref;
        use std::rc::Rc;

        pub enum FS<T> {
            One,
            Zero,
            Val(T),
            Add(FreeSemiring<T>, FreeSemiring<T>),
            Mul(FreeSemiring<T>, FreeSemiring<T>),
        }

        #[derive(Clone)]
        pub struct FreeSemiring<T> {
            rc: Rc<FS<T>>,
        }

        impl<T> Deref for FreeSemiring<T> {
            type Target = FS<T>;
            fn deref(&self) -> &Self::Target {
                &self.rc.deref()
            }
        }

        impl<T> From<T> for FreeSemiring<T> {
            fn from(t: T) -> FreeSemiring<T> {
                FreeSemiring { rc: Rc::new(FS::Val(t)) }
            }
        }

        impl<T> From<FS<T>> for FreeSemiring<T> {
            fn from(fs: FS<T>) -> FreeSemiring<T> {
                FreeSemiring { rc: Rc::new(fs) }
            }
        }

        impl<T: Clone> Semiring for FreeSemiring<T> {
            fn zero() -> Self {
                FreeSemiring::from(FS::Zero)
            }
            fn one() -> Self {
                FreeSemiring::from(FS::One)
            }
            fn add(&self, other: &Self) -> Self {
                free_operation(Op::Add, self, other)
            }
            fn mul(&self, other: &Self) -> Self {
                free_operation(Op::Mul, self, other)
            }
        }

        enum Op {
            Add,
            Mul,
        }

        fn free_operation<T: Clone>(
            op: Op,
            left: &FreeSemiring<T>,
            right: &FreeSemiring<T>,
        ) -> FreeSemiring<T> {
            let op = match op {
                Op::Add => FS::Add,
                Op::Mul => FS::Mul,
            };
            match **left {
                FS::Zero => right.clone(),
                _ => match **right {
                    FS::Zero => left.clone(),
                    _ => FreeSemiring::from(op(left.clone(), right.clone())),
                },
            }
        }

        impl<T: Copy> FreeSemiring<T> {
            pub fn eval<S, F>(&self, f: &F) -> S
            where
                S: Semiring,
                F: Fn(T) -> S,
            {
                match **self {
                    FS::Zero => Semiring::zero(),
                    FS::One => Semiring::one(),
                    FS::Val(t) => f(t),
                    FS::Add(ref left, ref right) => left.eval(f).add(&right.eval(f)),
                    FS::Mul(ref left, ref right) => left.eval(f).mul(&right.eval(f)),
                }
            }
        }

    }
}

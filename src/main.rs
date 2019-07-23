fn main() {
    use crate::semiring::Semiring;
    let from_string = semiring::free::FreeSemiring::from;
    let a = from_string("A");
    let b = from_string("B");
    let f = |s| match s {
        "A" => 1,
        "B" => 2,
        _ => 3,
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

        impl<T> From<T> for FreeSemiring<T> {
            fn from(t: T) -> FreeSemiring<T> {
                FreeSemiring {
                    rc: Rc::new(FS::Val(t)),
                }
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
                match (self.rc.deref(), other.rc.deref()) {
                    (FS::Zero, _) => other.clone(),
                    (_, FS::Zero) => self.clone(),
                    (_, _) => FreeSemiring::from(FS::Add(self.clone(), other.clone()))
                }
            }
            fn mul(&self, other: &Self) -> Self {
                match (self.rc.deref(), other.rc.deref()) {
                    (FS::One, _) => other.clone(),
                    (_, FS::One) => self.clone(),
                    (_, _) => FreeSemiring::from(FS::Mul(self.clone(), other.clone()))
                }
            }
        }

        impl<T: Copy> FreeSemiring<T> {
            pub fn eval<S, F>(&self, f: &F) -> S
            where
                S: Semiring,
                F: Fn(T) -> S,
            {
                match self.rc.deref() {
                    FS::Zero => Semiring::zero(),
                    FS::One => Semiring::one(),
                    FS::Val(t) => f(*t),
                    FS::Add(ref left, ref right) => left.eval(f).add(&right.eval(f)),
                    FS::Mul(ref left, ref right) => left.eval(f).mul(&right.eval(f)),
                }
            }
        }

        #[cfg(test)]
        mod tests {
            use super::*;
            #[test]
            fn free_semiring() {
                let from_string = FreeSemiring::from;
                let zero = FreeSemiring::zero();
                let one = FreeSemiring::one();
                let a = from_string("A");
                let b = from_string("B");
                let f = |s| match s {
                    "A" => 2,
                    "B" => 3,
                    _ => 7,
                };
                assert_eq!(a.add(&b).eval(&f), 5);
                assert_eq!(a.mul(&b).eval(&f), 6);
                assert_eq!(a.add(&zero).eval(&f), 2);
                assert_eq!(zero.add(&a).eval(&f), 2);
                assert_eq!(a.mul(&one).eval(&f), 2);
                assert_eq!(one.mul(&a).eval(&f), 2);
                assert_eq!(a.add(&one).eval(&f), 3);
            }
        }
    }
}

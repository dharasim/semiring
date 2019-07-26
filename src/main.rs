fn main() {}

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

    /// Core type to implement free semiring and semiring distribution
    pub enum FS<T, R> {
        One,
        Zero,
        Val(T),
        Add(R, R),
        Mul(R, R),
    }

    pub mod free {
        use super::{Semiring,FS};
        use std::ops::Deref;
        use std::rc::Rc;

        #[derive(Clone)]
        pub struct FreeSemiring<T> {
            rc: Rc<FS<T, FreeSemiring<T>>>,
        }

        impl<T> From<T> for FreeSemiring<T> {
            fn from(t: T) -> FreeSemiring<T> {
                FreeSemiring {
                    rc: Rc::new(FS::Val(t)),
                }
            }
        }

        impl<T> From<FS<T, FreeSemiring<T>>> for FreeSemiring<T> {
            fn from(fs: FS<T, FreeSemiring<T>>) -> FreeSemiring<T> {
                FreeSemiring { rc: Rc::new(fs) }
            }
        }

        impl<T: Clone> Semiring for FreeSemiring<T> {
            fn add(&self, other: &Self) -> Self {
                match (self.rc.deref(), other.rc.deref()) {
                    (FS::Zero, _) => other.clone(),
                    (_, FS::Zero) => self.clone(),
                    (_, _) => FreeSemiring::from(FS::Add(self.clone(), other.clone())),
                }
            }
            fn mul(&self, other: &Self) -> Self {
                match (self.rc.deref(), other.rc.deref()) {
                    (FS::One, _) => other.clone(),
                    (_, FS::One) => self.clone(),
                    (_, _) => FreeSemiring::from(FS::Mul(self.clone(), other.clone())),
                }
            }
            fn zero() -> Self {
                FreeSemiring::from(FS::Zero)
            }
            fn one() -> Self {
                FreeSemiring::from(FS::One)
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
    
    pub mod dist {
        use super::{Semiring,FS};
        use std::ops::Deref;
        use std::rc::Rc;
        use std::ops::Div;
        
        extern crate rand;
        use rand::distributions::{Bernoulli, Distribution};

        /// Helper function that draws a bool from a bernoulli distribution
        fn bernoulli(p: f64) -> bool {
            Bernoulli::new(p).unwrap().sample(&mut rand::thread_rng())
        }

        #[derive(Clone)]
        pub struct TreeDistribution<T, P> {
            rc: Rc<FS<T, TreeDistribution<T, P>>>,
            prob: P,
        }

        impl<T, P> From<(T, P)> for TreeDistribution<T, P> {
            fn from((t, p): (T, P)) -> TreeDistribution<T, P> {
                TreeDistribution {
                    rc: Rc::new(FS::Val(t)),
                    prob: p,
                }
            }
        }

        impl<T, P> Semiring for TreeDistribution<T, P>
        where
            T: Clone,
            P: Clone + Semiring,
        {
            fn add(&self, other: &Self) -> Self {
                match (self.rc.deref(), other.rc.deref()) {
                    (FS::Zero, _) => other.clone(),
                    (_, FS::Zero) => self.clone(),
                    (_, _) => TreeDistribution {
                        rc: Rc::new(FS::Add(self.clone(), other.clone())),
                        prob: self.prob.add(&other.prob),
                    },
                }
            }
            fn mul(&self, other: &Self) -> Self {
                match (self.rc.deref(), other.rc.deref()) {
                    (FS::One, _) => other.clone(),
                    (_, FS::One) => self.clone(),
                    (_, _) => TreeDistribution {
                        rc: Rc::new(FS::Mul(self.clone(), other.clone())),
                        prob: self.prob.mul(&other.prob),
                    },
                }
            }
            fn zero() -> Self {
                TreeDistribution {
                    rc: Rc::new(FS::Zero),
                    prob: Semiring::zero(),
                }
            }
            fn one() -> Self {
                TreeDistribution {
                    rc: Rc::new(FS::One),
                    prob: Semiring::one(),
                }
            }
        }

        impl<T, P> TreeDistribution<T, P>
        where
            T: Copy,
            for<'a> &'a P: Div,
            for<'a> f64: From<<&'a P as Div>::Output>,
        {
            pub fn sample(&self) -> Result<Vec<T>, String> {
                let mut ts = vec![];
                self.sample_and_push_to(&mut ts)?;
                Ok(ts)
            }
            fn sample_and_push_to(&self, ts: &mut Vec<T>) -> Result<(), String> {
                match self.rc.deref() {
                    FS::Zero => Err(String::from("Distribution over the empy set")),
                    FS::One => Ok(()),
                    FS::Val(t) => {
                        ts.push(*t);
                        Ok(())
                    }
                    FS::Add(ref left, ref right) => {
                        if bernoulli(left.prob.div(&self.prob).into()) {
                            left.sample_and_push_to(ts)
                        } else {
                            right.sample_and_push_to(ts)
                        }
                    }
                    FS::Mul(ref left, ref right) => {
                        left.sample_and_push_to(ts)?;
                        right.sample_and_push_to(ts)
                    }
                }
            }
        }
    }
}

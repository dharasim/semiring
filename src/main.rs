fn main() {
    let a = Foo { val: 3 };
    let b = a.foo();
    println!("{:?}", b);
}

struct Foo {
    val: i32,
}

impl Foo {
    fn foo(&self) -> i32 {
        self.val
    }
}

mod semiring {
    use std::ops::Add;
    use std::ops::Mul;

    trait Zero {
        type Output;
        fn zero() -> Self::Output;
    }

    trait One {
        type Output;
        fn one() -> Self::Output;
    }

    // Sized required by Add and Mul
    trait Semiring<Output = Self>:
        Sized
        + Add<Output = Output>
        + Mul<Output = Output>
        + Zero<Output = Output>
        + One<Output = Output>
    {
    }

    // type Interpretation = Fn T ->

    mod free {
        // use std::clone::Clone;
        use super::{Add,Mul,Zero,One,Semiring};
        use std::ops::Deref;
        use std::rc::Rc;

        enum FS<T> {
            One,
            Zero,
            Val(T),
            Add(FreeSemiring<T>, FreeSemiring<T>),
            Mul(FreeSemiring<T>, FreeSemiring<T>),
        }

        #[derive(Clone)]
        struct FreeSemiring<T> {
            rc: Rc<FS<T>>,
        }

        impl<T> Deref for FreeSemiring<T> {
            type Target = FS<T>;
            fn deref(&self) -> &Self::Target {
                &self.rc.deref()
            }
        }

        impl<T> From<FS<T>> for FreeSemiring<T> {
            fn from(fs: FS<T>) -> FreeSemiring<T> {
                FreeSemiring { rc: Rc::new(fs) }
            }
        }

        impl<T> Zero for &FreeSemiring<T> {
            type Output = FreeSemiring<T>;
            fn zero() -> Self::Output {
                FreeSemiring::from(FS::Zero)
            }
        }

        impl<T> One for &FreeSemiring<T> {
            type Output = FreeSemiring<T>;
            fn one() -> Self::Output {
                FreeSemiring::from(FS::One)
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

        // impl<T: Clone> Add for FreeSemiring<T>

        impl<T: Clone> Add for &FreeSemiring<T> {
            type Output = FreeSemiring<T>;
            fn add(self, other: Self) -> Self::Output {
                free_operation(Op::Add, self, other)
            }
        }

        impl<T: Clone> Mul for &FreeSemiring<T> {
            type Output = FreeSemiring<T>;
            fn mul(self, other: Self) -> Self::Output {
                free_operation(Op::Mul, self, other)
            }
        }

        impl<T: Clone> Semiring<FreeSemiring<T>> for &FreeSemiring<T> {}

        // impl<T: Clone + Sized> FreeSemiring<T> {
        //     fn eval<S: Sized>(&self, interpretation: Fn(T) -> S) -> S {
        //         interpretation(&Self::zero())
        //     }
        // }

    }
}

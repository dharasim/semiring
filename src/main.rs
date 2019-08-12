#![allow(dead_code)]

fn main() {}

mod lazymap {
    use std::collections::HashMap;
    use std::hash::Hash;

    struct LazyMap<K,V,F> {
        map: HashMap<K,V>,
        f: F,
    }

    impl<K: Eq + Hash + Copy, V, F: Fn(&K) -> Option<V>> LazyMap<K,V,F> {
        fn get(&mut self, k: &K) -> Option<&V> {
            if !self.map.contains_key(k) {
                let v = (self.f)(k)?;
                self.map.insert(*k, v);
            }
            self.map.get(k)
        }
    }

    #[test]
    fn fibo_test() {
        let mut fibs = HashMap::new();
        fibs.insert(0, 0);
        fibs.insert(1, 1);

        let big_fib = fibo(&mut fibs, &50);
        println!("fibo(50) = {}", big_fib);

        let true_fibs = [0,1,1,2,3,5,8,13,21,34,55,89,144].to_vec();
        let my_fibs: Vec<i64> = (0..true_fibs.len()).map(|k| fibo(&mut fibs, &(k as i64))).collect();
        assert_eq!(
            true_fibs,
            my_fibs
        );

    }

    fn fibo(map: &mut HashMap<i64,i64>, k: &i64) -> i64 {
        if !map.contains_key(k) {
            let n = fibo(map, &(k-1)) + fibo(map, &(k-2));
            map.insert(*k, n);
            n
        } else {
            *map.get(k).unwrap()
        }
    }
}

mod parser {
    use std::hash::Hash;
    use std::collections::HashMap;
    use crate::semiring::Semiring;

    pub trait Parser {
        type Category: Clone + Eq + Hash;
        type Score: Semiring;
        fn start(&self) -> &Self::Category;
        fn is_terminal(&self, c: &Self::Category) -> bool;
        fn unary_completions(&self, rhs: &Self::Category) -> HashMap<Self::Category, Self::Score>;
        fn binary_completions(
            &self,
            rhs1: &Self::Category,
            rhs2: &Self::Category,
        ) -> HashMap<Self::Category, Self::Score>;

        fn parse(&self, terminals: &[Self::Category]) -> Self::Score {
            let n = terminals.len();
            let mut chart: Vec<Vec<HashMap<Self::Category,Self::Score>>> = vec![vec![HashMap::new(); n]; n];

            for (i, t) in terminals.iter().enumerate() {
                chart[i][i].insert(t.clone(), Semiring::one());
                union_with_mut(&mut chart[i][i], self.unary_completions(t), Semiring::add)
            }

            for l in 1..n {
                for i in 0..n-l {
                    let j = i + l;
                    for k in i..j {
                        // split mutable ref to chart into multiple mutable refs of cells
                        // kp1 means k+1
                        let (cells_start_in_i, cells_start_in_kp1) = get2_mut(&mut chart, i, k+1);
                        let (cell_ik, cell_ij) = get2_mut(cells_start_in_i, k, j);
                        let cell_kp1j = &cells_start_in_kp1[j];

                        for (rhs1, s1) in cell_ik.iter() {
                            for (rhs2, s2) in cell_kp1j.iter() {
                                for (lhs, s) in self.binary_completions(rhs1, rhs2) {
                                    if let Some(ss) = cell_ij.get_mut(&lhs) {
                                        *ss = ss.add(&s.mul(s1).mul(s2));
                                    } else {
                                        cell_ij.insert(lhs, s.mul(s1).mul(s2));
                                    }
                                }
                            }
                        }
                    }
                }
            }

            chart[0][n-1].remove(self.start()).unwrap_or(Semiring::zero())
        }
    }

    fn union_with_mut<K,V,F>(one: &mut HashMap<K,V>, two: HashMap<K,V>, f: F) 
    where K: Eq + Hash, F: Fn(&V,&V) -> V {
        for (k, v2) in two {
            if let Some(v1) = one.get_mut(&k) {
                *v1 = f(&v1, &v2);
            } else {
                one.insert(k, v2);
            }
        }
    }

    #[test]
    fn test_union_with_mut() {
        let mut map1: HashMap<_,_> = [(1, 2), (3, 4)].to_vec().into_iter().collect();
        let map2: HashMap<_,_> = [(1, 10), (5, 6)].to_vec().into_iter().collect();
        union_with_mut(&mut map1, map2, |a,b| a+b);
        assert_eq!(map1, [(1, 12), (3, 4), (5, 6)].to_vec().into_iter().collect());
    }

    fn get2_mut<T>(slice: &mut [T], i: usize, j: usize) -> (&mut T, &mut T) {
        let (_part1, rest) = slice.split_at_mut(i);
        let (part2, part3) = rest.split_at_mut(j-i);
        (part2.first_mut().unwrap(), part3.first_mut().unwrap())
    }

    #[test]
    fn test_get2_mut() {
        let mut v = [0,10,20,30,40];
        let i = 1;
        let j = 3;
        let (a, b) = get2_mut(&mut v, i, j);
        *a += i;
        *b += j;
        assert_eq!(v, [0,11,20,33,40]);
    }

    #[test]
    fn test_parse() {
        #[derive(PartialEq, Eq, Hash, Copy, Clone)]
        enum Symbol { Start, Leaf };
        use Symbol::{Start, Leaf};

        struct BinaryTreeCountParser;

        impl Parser for BinaryTreeCountParser {
            type Category = Symbol;
            type Score = i32;
            fn start(&self) -> &Self::Category {
                &Start
            }
            fn is_terminal(&self, c: &Self::Category) -> bool {
                c == &Leaf
            }
            fn unary_completions(&self, rhs: &Self::Category) -> HashMap<Self::Category, Self::Score> {
                match rhs {
                    Start => HashMap::new(),
                    Leaf => [(Start, 1)].to_vec().into_iter().collect()
                }
            }
            fn binary_completions(
                &self,
                rhs1: &Self::Category,
                rhs2: &Self::Category,
            ) -> HashMap<Self::Category, Self::Score> {
                match (rhs1, rhs2) {
                    (Start, Start) => [(Start, 1)].to_vec().into_iter().collect(),
                    _ => HashMap::new()
                }
            }
        }

        let numbers_of_binary_trees: Vec<_> = (1..=10).into_iter().map(|n| BinaryTreeCountParser.parse(&vec![Leaf; n])).collect();
        let catalan_numbers = [1, 1, 2, 5, 14, 42, 132, 429, 1430, 4862].to_vec();
        assert_eq!(
            numbers_of_binary_trees,
            catalan_numbers
        )
    }
}

mod semiring {
    pub trait Semiring: Clone {
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
        use super::{Semiring, FS};
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
        use super::{Semiring, FS};
        use std::ops::Deref;
        use std::ops::Div;
        use std::rc::Rc;

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

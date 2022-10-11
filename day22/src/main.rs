// Solution is heavily(!) inspired by https://blog.jle.im/entry/shuffling-things-up.html#a-first-guess-at-implementation

use std::{ops::{Add, Mul}};

/// A shuffle move
/// 
/// Note that all the moves that can occur may be describe by an transformation of kind
/// ```
/// a * x + b mod m
/// ```
/// where
/// - `x` is the position of a card in the deck
/// - `m` is the size of the deck
/// 
/// Because this statement holds for all moves, it is possible two compose two moves
/// ```
/// h = g . f
/// ```
/// where
/// - `g(x) = a * x + b mod m`
/// - `f(x) = a' * x + b' mod m`
/// - `c(x) = a * (a' * x + b') + b = (a * a' mod m) * x + (a * b' + b mod m)`
/// 
/// Also, as every move describe a permutation, which is invertable, so that
/// 
/// ```
/// f^-1 . f = id := 1 * x + 0 mod m
/// f^-1(f(x)) = (a' * a) * x + (a' * b + b')
/// ```
/// where
/// - `f^-1(x) = a' * x + b' mod m`
/// - `f(x) = a * x + b mod m`
/// 
/// Thereby follows
/// ```
/// a ^ m = a mod m
/// a ^ (m - 1) * a = a mod m
/// a ^ (m - 1) = 1 mod m
/// a ^ (m - 2) * a = 1 mod m
/// 
/// =>
/// a' * a = 1 mod m <=> a' = a ^ (m - 2) mod m
/// ```
/// 
/// and
/// ```
/// a' * b + b' = 0 mod m <=> b' = -1 * a' * b
/// ```
#[derive(Debug, Clone)]
enum Move<const N: u128> {
    Stack,
    Cut(i128),
    Increment(i128),
    Identity,
    /// A move that moves every element to place `a * x + b mod N` 
    Custom(i128, i128),
}

impl<const N: u128> Move<N> {
    fn apply(&self, i: i128) -> i128 {
        let (a,b) = self.get_params();
        (a * i + b).rem_euclid(N as i128)
    }

    fn invert(&self) -> Self {
        let (a,b) = self.get_params();
        let inverted_a = pow_mod(a, N as i128 - 2, N);
        let inverted_b = -(inverted_a * b);
        Move::Custom(inverted_a, inverted_b)
    }

    fn get_params(&self) -> (i128, i128) {
        match self {
            Move::Stack => (-1, - 1),
            Move::Cut(n) => (1, - *n),
            Move::Increment(n) => (*n, 0),
            Move::Identity => (1, 0),
            Move::Custom(a, b) => (*a, *b),
        }
    }
}

/// Combines two moves into one
/// 
/// Example
/// let a: Move = …
/// let b: Move = …
/// let c = a + b;
/// b.apply(a.apply(x)) = c.apply(x)
impl<const N: u128> Add<Move<N>> for Move<N> {
    type Output = Move<N>;

    fn add(self, rhs: Move<N>) -> Self::Output {
        let (a,b) = self.get_params();
        let (c,d) = rhs.get_params();
        let new_a =
            c.checked_mul(a).map(|x| x.rem_euclid(N as i128))
                .unwrap_or_else(|| mul_mod(a,c,N));
        let new_b =
            d + c.checked_mul(b).map(|x| x.rem_euclid(N as i128))
                    .unwrap_or_else(|| mul_mod(c,b,N));

        Move::Custom(new_a, new_b)
    }
}

/// 'Chains' a move `n` times with itself
/// 
/// Example:
/// let a: Move = Move::Custom(1, 0);
/// a + a + a == a * 3
impl<const N: u128> Mul<u128> for Move<N> {
    type Output = Move<N>;

    fn mul(self, rhs: u128) -> Self::Output {
        if rhs == 1 {
            self
        } else if rhs % 2 == 1 {
            self.clone() + ((self.clone() + self) * (rhs / 2))
        } else {
            (self.clone() + self) * (rhs / 2)
        }
    }
}


fn main() {
    let input = std::fs::read_to_string("input.txt").unwrap();
    let moves = parse_input::<10007>(input.clone());

    let result = part1(&moves, 2019);
    println!("Part 1: {}", result);


    let moves = parse_input::<119315717514047>(input);
    let result = part2(&moves, 101741582076661, 2020);
    println!("Part 2: {}", result);
}

/// Combines all moves into one move and applies it onto `pos`
fn part1<const N: u128>(moves: &[Move<N>], pos: i128) -> i128 {
    let shuffle: Move<N> = moves.iter().cloned().fold(Move::Identity, |a,b| a + b);
    shuffle.apply(pos)
}

/// Combines all moves into one move, inverts it, repeats it `times` times and applies it onto `pos`
fn part2<const N: u128>(moves: &[Move<N>], times: u128, pos: i128) -> i128 {
    let shuffle: Move<N> = moves.iter().cloned().fold(Move::Identity, |a,b| a + b).invert() * times;
    shuffle.apply(pos)
}

fn parse_input<const N: u128>(input: String) -> Vec<Move<N>> {
    input
        .lines()
        .map(|s| {
            match s.split(' ').collect::<Vec<&str>>()[..] {
                ["deal", "into", "new", "stack"] => Move::Stack,
                ["deal", "with", "increment", i] => Move::Increment(i.parse().unwrap()),
                ["cut", i] => Move::Cut(i.parse().unwrap()),
                _ => panic!("Read bad line '{}'", s)
            }
        })
        .collect::<Vec<Move<N>>>()
}

/// Calcuates `a ^ e mod m`
fn pow_mod(a: i128, e: i128, m: u128) -> i128 {
    if e == 0 {
        1
    } else if e % 2 == 1 {
        (a * pow_mod((a * a) % m as i128, e / 2, m)) % m as i128
    } else {
        (pow_mod((a * a) % m as i128, e / 2, m)) % m as i128
    }
}

/// Calcuates `a * b mod m`
fn mul_mod(a: i128, mut b: i128, m: u128) -> i128 {
    // Example / basic idea:
    // > 5 * 4 mod m = (5 * 2 mod m) * 2 mod m
    // > 5 * (4 + 1) mod m = 5 * 4 mod m + 5

    let mut sign = 1;
    if b < 0 {
        sign = -1;
        b = b.abs();
    }

    if b == 1 {
        a
    } else if b % 2 == 0 {
        sign * mul_mod((a * 2).rem_euclid(m as i128), b / 2, m)
    } else {
        sign * (a + mul_mod(a, b - 1, m))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test(input: String, l: i128) -> Vec<i128> {
        let moves = parse_input::<10>(input);

        let mut result = vec![0; l as usize];
        for i in 0..l {
            let num_result_pos = part1(&moves, i);
            result[num_result_pos as usize] = i;
        }

        result
    }

    #[test]
    fn test_stack() {
        let result = test("deal into new stack".to_owned(), 10);
       assert_eq!(result, vec![9,8,7,6,5,4,3,2,1,0]);
    }

    #[test]
    fn test_cut_positive() {
        let result = test("cut 3".to_owned(), 10);
        assert_eq!(result, vec![3,4,5,6,7,8,9,0,1,2]);
    }

    #[test]
    fn test_cut_negative() {
        let result = test("cut -4".to_owned(), 10);
        assert_eq!(result, vec![6,7,8,9,0,1,2,3,4,5]);
    }

    #[test]
    fn test_increment() {
        let result = test("deal with increment 3".to_owned(), 10);
        assert_eq!(result, vec![0,7,4,1,8,5,2,9,6,3]);
    }

}
use std::env;

const M: i64 = 2147483647;
const MASK: i64 = 0b1111111111111111;
const FACTOR_A: i64 = 16807;
const FACTOR_B: i64 = 48271;

fn gen_next(factor: i64, current: i64) -> i64 {
    return (current * factor) % M;
}

fn gen_next_picky(factor: i64, current: i64, mult: i64) -> i64 {
    let mut next = gen_next(factor, current);
    while next % mult != 0 {
        next = gen_next(factor, next);
    }
    return next;
}

fn duel<F, G>(n: i64, next_a: F, mut value_a: i64, next_b: G, mut value_b: i64) -> i64
where
    F: Fn(i64) -> i64,
    G: Fn(i64) -> i64,
{
    let mut count = 0;

    for _ in 0..n {
        value_a = next_a(value_a);
        value_b = next_b(value_b);
        if (value_a & MASK) == (value_b & MASK) {
            count += 1;
        }
    }

    return count;
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let start_a: i64 = args[1].parse().unwrap();
    let start_b: i64 = args[2].parse().unwrap();

    println!(
        "Duel 1: {}",
        duel(
            40000000,
            |n| gen_next(FACTOR_A, n),
            start_a,
            |n| gen_next(FACTOR_B, n),
            start_b,
        )
    );
    println!(
        "Duel 2: {}",
        duel(
            5000000,
            |n| gen_next_picky(FACTOR_A, n, 4),
            start_a,
            |n| gen_next_picky(FACTOR_B, n, 8),
            start_b,
        )
    );
}

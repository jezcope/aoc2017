use std::io;
use std::env;
use std::io::BufRead;
use std::collections::HashMap;

#[derive(PartialEq, Clone, Copy, Debug)]
enum Direction {Up, Right, Down, Left}
#[derive(PartialEq, Clone, Copy, Debug)]
enum Infection {Clean, Weakened, Infected, Flagged}

use self::Direction::*;
use self::Infection::*;

type Grid = HashMap<(isize, isize), Infection>;

fn turn_left(d: Direction) -> Direction {
    match d {Up => Left, Right => Up, Down => Right, Left => Down}
}

fn turn_right(d: Direction) -> Direction {
    match d {Up => Right, Right => Down, Down => Left, Left => Up}
}

fn turn_around(d: Direction) -> Direction {
    match d {Up => Down, Right => Left, Down => Up, Left => Right}
}

fn make_move(d: Direction, x: isize, y: isize) -> (isize, isize) {
    match d {
        Up    => (x-1, y),
        Right => (x, y+1),
        Down  => (x+1, y),
        Left  => (x, y-1),
    }
}

fn basic_step(grid: &mut Grid, x: &mut isize, y: &mut isize, d: &mut Direction) -> usize {
    let mut infect = 0;
    let current = match grid.get(&(*x, *y)) {
        Some(v) => *v,
        None => Clean,
    };
    if current == Infected {
        *d = turn_right(*d);
    } else {
        *d = turn_left(*d);
        infect = 1;
    };
    grid.insert((*x, *y), match current {
        Clean => Infected,
        Infected => Clean,
        x => panic!("Unexpected infection state {:?}", x),
    });
    let new_pos = make_move(*d, *x, *y);
    *x = new_pos.0;
    *y = new_pos.1;
    
    infect
}

fn nasty_step(grid: &mut Grid, x: &mut isize, y: &mut isize, d: &mut Direction) -> usize {
    let mut infect = 0;
    let new_state: Infection;
    let current = match grid.get(&(*x, *y)) {
        Some(v) => *v,
        None => Infection::Clean,
    };
    match current {
        Clean => {
            *d = turn_left(*d);
            new_state = Weakened;
        },
        Weakened => {
            new_state = Infected;
            infect = 1;
        },
        Infected => {
            *d = turn_right(*d);
            new_state = Flagged;
        },
        Flagged => {
            *d = turn_around(*d);
            new_state = Clean;
        }
    };
    grid.insert((*x, *y), new_state);
    let new_pos = make_move(*d, *x, *y);
    *x = new_pos.0;
    *y = new_pos.1;
    
    infect
}

fn virus_infect<F>(mut grid: Grid, mut step: F, mut x: isize, mut y: isize, mut d: Direction, n: usize) -> usize
    where F: FnMut(&mut Grid, &mut isize, &mut isize, &mut Direction) -> usize,
{
    (0..n).map(|_| step(&mut grid, &mut x, &mut y, &mut d))
        .sum()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let n_basic: usize = args[1].parse().unwrap();
    let n_nasty: usize = args[2].parse().unwrap();
    
    let stdin = io::stdin();
    let lines: Vec<String> = stdin.lock()
        .lines()
        .map(|x| x.unwrap())
        .collect();
    let mut grid: Grid = HashMap::new();
    let x0 = (lines.len() / 2) as isize;
    let y0 = (lines[0].len() / 2) as isize;

    for (i, line) in lines.iter().enumerate() {
        for (j, c) in line.chars().enumerate() {
            grid.insert((i as isize, j as isize),
                        match c {'#' => Infected, _ => Clean});
        }
    }

    let basic_steps = virus_infect(grid.clone(), basic_step, x0, y0, Up, n_basic);
    println!("Basic: infected {} times", basic_steps);

    let nasty_steps = virus_infect(grid, nasty_step, x0, y0, Up, n_nasty);
    println!("Nasty: infected {} times", nasty_steps);
}

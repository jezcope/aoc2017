use std::io;
use std::env;
use std::io::BufRead;
use std::collections::HashMap;

#[derive(PartialEq, Clone, Copy, Debug)]
enum Direction {Up, Right, Down, Left}
#[derive(PartialEq, Clone, Copy, Debug)]
enum Infection {Clean, Weakened, Infected, Flagged}

type Grid = HashMap<(isize, isize), Infection>;

fn turn_left(d: &Direction) -> Direction {
    match *d {
        Direction::Up    => Direction::Left,
        Direction::Right => Direction::Up,
        Direction::Down  => Direction::Right,
        Direction::Left  => Direction::Down,
    }
}

fn turn_right(d: &Direction) -> Direction {
    match *d {
        Direction::Up    => Direction::Right,
        Direction::Right => Direction::Down,
        Direction::Down  => Direction::Left,
        Direction::Left  => Direction::Up,
    }
}

fn turn_around(d: &Direction) -> Direction {
    match *d {
        Direction::Up    => Direction::Down,
        Direction::Right => Direction::Left,
        Direction::Down  => Direction::Up,
        Direction::Left  => Direction::Right,
    }
}

fn make_move(d: &Direction, x: isize, y: isize) -> (isize, isize) {
    match *d {
        Direction::Up    => (x-1, y),
        Direction::Right => (x, y+1),
        Direction::Down  => (x+1, y),
        Direction::Left  => (x, y-1),
    }
}

fn basic_step(grid: &mut Grid, x: &mut isize, y: &mut isize, d: &mut Direction) -> usize {
    let mut infect = 0;
    let current = match grid.get(&(*x, *y)) {
        Some(v) => *v,
        None => Infection::Clean,
    };
    if current == Infection::Infected {
        *d = turn_right(d);
    } else {
        *d = turn_left(d);
        infect = 1;
    };
    grid.insert((*x, *y), match current {
        Infection::Clean => Infection::Infected,
        Infection::Infected => Infection::Clean,
        x => panic!("Unexpected infection state {:?}", x),
    });
    let new_pos = make_move(d, *x, *y);
    *x = new_pos.0;
    *y = new_pos.1;
    
    return infect;
}

fn nasty_step(grid: &mut Grid, x: &mut isize, y: &mut isize, d: &mut Direction) -> usize {
    let mut infect = 0;
    let new_state: Infection;
    let current = match grid.get(&(*x, *y)) {
        Some(v) => *v,
        None => Infection::Clean,
    };
    match current {
        Infection::Clean => {
            *d = turn_left(d);
            new_state = Infection::Weakened;
        },
        Infection::Weakened => {
            new_state = Infection::Infected;
            infect = 1;
        },
        Infection::Infected => {
            *d = turn_right(d);
            new_state = Infection::Flagged;
        },
        Infection::Flagged => {
            *d = turn_around(d);
            new_state = Infection::Clean;
        }
    };
    grid.insert((*x, *y), new_state);
    let new_pos = make_move(d, *x, *y);
    *x = new_pos.0;
    *y = new_pos.1;
    
    return infect;
}

fn virus_infect<F>(mut grid: Grid, mut step: F, mut x: isize, mut y: isize, mut d: Direction, n: usize) -> usize
    where F: FnMut(&mut Grid, &mut isize, &mut isize, &mut Direction) -> usize,
{
    let mut total = 0;
    
    for _ in 0..n {
        total += step(&mut grid, &mut x, &mut y, &mut d);
    }

    return total;
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
            grid.insert((i as isize, j as isize), match c {
                '#' => Infection::Infected,
                _   => Infection::Clean,
            });
        }
    }

    let basic_steps = virus_infect(grid.clone(), basic_step, x0, y0, Direction::Up, n_basic);
    println!("Basic: infected {} times", basic_steps);

    let nasty_steps = virus_infect(grid, nasty_step, x0, y0, Direction::Up, n_nasty);
    println!("Nasty: infected {} times", nasty_steps);
}

use std::io;
use std::io::BufRead;

const ALPHA: &'static str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

fn change_direction(dia: &Vec<Vec<u8>>, x: usize, y: usize, dx: &mut i32, dy: &mut i32) {
    assert_eq!(dia[x][y], b'+');
    
    if dx.abs() == 1 {
        *dx = 0;
        if y + 1 < dia[x].len() && (dia[x][y + 1] == b'-' || ALPHA.contains(dia[x][y + 1] as char)) {
            *dy = 1;
        } else if dia[x][y - 1] == b'-' || ALPHA.contains(dia[x][y - 1] as char) {
            *dy = -1;
        } else {
            panic!("Huh? {} {}", dia[x][y+1] as char, dia[x][y-1] as char);
        }
    } else {
        *dy = 0;
        if x + 1 < dia.len() && (dia[x + 1][y] == b'|' || ALPHA.contains(dia[x + 1][y] as char)) {
            *dx = 1;
        } else if dia[x - 1][y] == b'|' || ALPHA.contains(dia[x - 1][y] as char) {
            *dx = -1;
        } else {
            panic!("Huh?");
        }
    }
}

fn follow_route(dia: Vec<Vec<u8>>) -> (String, i32) {
    let mut x: i32 = 0;
    let mut y: i32;
    let mut dx: i32 = 1;
    let mut dy: i32 = 0;
    let mut result = String::new();
    let mut steps = 1;

    match dia[0].iter().position(|x| *x == b'|') {
        Some(i) => y = i as i32,
        None => panic!("Could not find '|' in first row"),
    }

    loop {
        x += dx;
        y += dy;
        match dia[x as usize][y as usize] {
            b'A'...b'Z' => result.push(dia[x as usize][y as usize] as char),
            b'+' => change_direction(&dia, x as usize, y as usize, &mut dx, &mut dy),
            b' ' => return (result, steps),
            _ => (),
        }
        steps += 1;
    }
}

fn main() {
    let stdin = io::stdin();
    let lines: Vec<Vec<u8>> = stdin.lock().lines()
        .map(|l| l.unwrap().into_bytes())
        .collect();

    let result = follow_route(lines);
    println!("Route: {}", result.0);
    println!("Steps: {}", result.1);
}

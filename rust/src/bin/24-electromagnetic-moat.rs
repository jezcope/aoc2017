use std::io;
use std::fmt;
use std::io::BufRead;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Component(u8, u8);

#[derive(Debug, Copy, Clone, Default)]
struct BridgeResult {
    strength: u16,
    length: u16,
}

impl Component {
    fn from_str(s: &str) -> Component {
        let parts: Vec<&str> = s.split('/').collect();
        assert!(parts.len() == 2);
        Component(parts[0].parse().unwrap(), parts[1].parse().unwrap())
    }

    fn fits(self, port: u8) -> bool {
        self.0 == port || self.1 == port
    }

    fn other_end(self, port: u8) -> u8 {
        if self.0 == port {
            return self.1;
        } else if self.1 == port {
            return self.0;
        } else {
            panic!("{} doesn't fit port {}", self, port);
        }
    }

    fn strength(self) -> u16 {
        self.0 as u16 + self.1 as u16
    }
}

impl fmt::Display for Component {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.0, self.1)
    }
}

impl fmt::Display for BridgeResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(S: {}, L: {})", self.strength, self.length)
    }
}

fn best_bridge<F>(port: u8, key: &F, components: &Vec<Component>) -> Option<BridgeResult>
    where F: Fn(&BridgeResult) -> u16
{
    if components.len() == 0 {
        return None;
    }
    
    components.iter()
        .filter(|c| c.fits(port))
        .map(|c| {
            let b = best_bridge(c.other_end(port), key,
                                &components.clone().into_iter()
                                .filter(|x| x != c).collect())
                .unwrap_or_default();
             BridgeResult{strength: c.strength() + b.strength,
                          length: 1 + b.length}
        })
        .max_by_key(key)
}

fn main() {
    let stdin = io::stdin();
    let components: Vec<_> = stdin.lock()
        .lines()
        .map(|l| Component::from_str(&l.unwrap()))
        .collect();

    match best_bridge(0, &|b: &BridgeResult| b.strength, &components) {
        Some(b) => println!("Strongest bridge is {}", b),
        None => println!("No strongest bridge found")
    };
    match best_bridge(0, &|b: &BridgeResult| b.length, &components) {
        Some(b) => println!("Longest bridge is {}", b),
        None => println!("No longest bridge found")
    };
}

use core::panic;
use std::{collections::{HashMap, HashSet}};

type Coord = (i8, i8);
type Map = HashMap<Coord, bool>;
type Level = i8;

fn main() {
    let path = std::env::args().collect::<Vec<String>>().get(1).cloned().unwrap();
    let map = parse_input(&path);
    let result1 = part1(map.clone());
    println!("Part 1: {}", result1);

    let result2 = part2(map);
    println!("Part 2: {}", result2);
}

fn part1(mut map: Map) -> u64 {
    let mut values = HashSet::new();
    values.insert(map_value(&map));

    loop {
        map = evolve(map);
        let value = map_value(&map);
        if values.contains(&value) {
            return value;
        } else {
            values.insert(value);
        }
    }
}

// PART 1

fn evolve(map: Map) -> Map {
    let mut new_map = HashMap::new();
    for y in 0..5 {
        for x in 0..5 {
            if let Some(b) = map.get(&(y, x)) {
                let neighbour_count = neighbours((y, x))
                    .iter()
                    .filter_map(|coord| map.get(coord).cloned())
                    .filter(|b| *b)
                    .count();
                let new_b = if *b {
                    neighbour_count == 1
                } else {
                    neighbour_count == 1 || neighbour_count == 2
                };

                new_map.insert((y,x), new_b);
            } else {
                panic!("Bad coordinate: {:?}", (y,x));
            }
        }
    }

    new_map
}

fn map_value(map: &Map) -> u64 {
    map.iter()
        .filter_map(|((y, x), value)| {
            if *value {
                Some(2u64.pow((x + y * 5) as u32))
            } else {
                None
            }
        })
        .sum()
}

// PART 2

struct Maps(HashMap<Level, Map>);

impl Maps {
    fn get_neighbour_count(&self, coord: Coord, level: Level) -> u8 {
        if matches!(self.0.get(&level), None) {
            return 0;
        }

        Maps::get_neighbours(coord, level)
            .iter()
            .map(|(coord, level)| {
                if self.get_cell(*coord, *level) {
                    1
                } else {
                    0
                }
            })
            .sum()
    }

    fn get_cell(&self, coord: Coord, level: Level) -> bool {
        assert_ne!(coord, (2,2));

        if matches!(self.0.get(&level), None) {
            false
        } else {
            *self.0.get(&level).unwrap().get(&coord).unwrap()
        }

    }

    fn get_neighbours(coord: Coord, level: Level) -> Vec<(Coord, Level)> {
        let mut result = Vec::new();
        let ns = neighbours(coord);
        for (y,x) in ns {
            if x == -1 {
                result.push(((2, 1), level - 1));
            } else if x == 5 {
                result.push(((2, 3), level - 1));
            } else if y == -1 {
                result.push(((1, 2), level - 1));
            } else if y == 5 {
                result.push(((3, 2), level - 1));
            } else if y == 2 && x == 2 {
                match coord {
                    (1,2) => {
                        for x in 0..5 {
                            result.push(((0, x), level + 1));
                        }
                    }
                    (3,2) => {
                        for x in 0..5 {
                            result.push(((4, x), level + 1));
                        }
                    }
                    (2,1) => {
                        for y in 0..5 {
                            result.push(((y, 0), level + 1));
                        }
                    }
                    (2,3) => {
                        for y in 0..5 {
                            result.push(((y, 4), level + 1));
                        }
                    }
                    _ => panic!("Got (2,2) as neighbour of {:?}", coord)
                }
            } else {
                result.push(((y,x), level));
            } 
        }

        result
    }

    fn has_level(&self, level: Level) -> bool {
        self.0.contains_key(&level)
    }

    fn create_level(&mut self, level: Level) {
        let mut map = HashMap::new();
        for y in 0..5 {
            for x in 0..5 {
                map.insert((y,x), false);
            }
        }

        self.0.insert(level, map);
    }

    fn bug_count(&self) -> u64 {
        self.0.iter()
            .map(|(_level, map)| {
                map.iter()
                    .map(|(coord, b)| {
                        if *coord != (2,2) && *b { 1u64 } else { 0 }
                    })
                    .sum::<u64>()
            })
            .sum()
    }
}

fn part2(map: Map) -> u64 {
    let mut maps = {
        let mut internal_map = HashMap::new();
        internal_map.insert(0, map);
        Maps(internal_map)
    };

    // print_maps(&maps);
    for _ in 0..200 {
        evolve_recursive(&mut maps, 0, Direction::Both);
    }

    print_maps(&maps);

    maps.bug_count()
}

#[derive(PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Both,
}

fn evolve_recursive(maps: &mut Maps, level: i8, direction: Direction) -> &Maps {
    use Direction::*;

    if !maps.has_level(level) {
        maps.create_level(level);
    }

    let mut new_map = HashMap::new();

    for y in 0..5 {
        for x in 0..5 {
            if (y,x) == (2,2) {
                continue;
            }
            let b = maps.get_cell((y,x), level);
            let neighbour_count = maps.get_neighbour_count((y,x), level);
            let new_b = if b {
                neighbour_count == 1
            } else {
                neighbour_count == 1 || neighbour_count == 2
            };

            new_map.insert((y,x), new_b);
        }
    }

    let value = map_value(&new_map);
    if direction == Both || direction == Down && (value != 0 || maps.has_level(level + 1)) {
        evolve_recursive(maps, level + 1, Down);
    }

    if direction == Both || direction == Up && (value != 0 || maps.has_level(level - 1)) {
        evolve_recursive(maps, level - 1, Up);
    }

    maps.0.insert(level, new_map);

    maps
}

// COMMON

fn parse_input(path: &str) -> Map {
    let mut map = HashMap::new();
    let input = std::fs::read_to_string(path).unwrap();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            map.insert((y as i8, x as i8), c == '#');
        }
    }

    map
}

fn neighbours(coord: Coord) -> [Coord; 4] {
    let (y, x) = coord;
    [(y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1)]
}

#[allow(dead_code)]
fn print_map(map: &Map, recursive: bool) {
    for y in 0..5 {
        for x in 0..5 {
            if recursive && (y,x) == (2,2) {
                print!("?");
            } else {
                print!("{}", if *map.get(&(y,x)).unwrap() { '#' } else { '.' });
            }
        }
        println!();
    }
    println!();
}

#[allow(dead_code)]
fn print_maps(maps: &Maps) {
    let mut keys = maps.0.keys().cloned().collect::<Vec<Level>>();
    keys.sort_unstable();

    for key in keys {
        println!("Level {}", key);
        print_map(maps.0.get(&key).unwrap(), true);
    }
}
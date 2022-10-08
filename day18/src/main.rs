use std::{collections::{HashMap, HashSet, VecDeque}};

type Coord = (u32, u32);

fn neighbours(coord: Coord) -> Vec<Coord> {
    let (y,x) = coord;
    if y > 0 && x > 0 {
        vec![(y-1,x), (y+1,x), (y,x-1), (y,x+1)]
    } else if y > 0 {
        vec![(y-1,x), (y+1,x), (y,x+1)]
    } else if x > 0 {
        vec![(y+1,x), (y,x-1), (y,x+1)]
    } else {
        vec![(y+1,x), (y,x+1)]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Tile {
    Wall,
    Floor,
    Key(char),
    Door(char)
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    let path = args.get(1).unwrap();
    let input = std::fs::read_to_string(path).unwrap();
    let (mut map, position) = build_map(input);

    let mut key_distance_map = build_key_distance_map(&map, &[position]);
    let keys_needed: HashSet<char> = map
        .iter()
        .filter_map(|(_, tile)| {
            match tile {
                Tile::Key(c) if c.is_ascii_alphabetic() => Some(c),
                _ => None
            }
        })
        .cloned()
        .collect();

    // run part 1

    let mut dp_map = HashMap::new();
    let result = solve1(&mut key_distance_map, &mut dp_map, '1', HashSet::new(), keys_needed.clone());
    println!("Part 1: {}", result);

    // prepare map for part 2

    let (x,y) = position;

    map.insert((x,y), Tile::Wall);
    map.insert((x - 1,y), Tile::Wall);
    map.insert((x + 1,y), Tile::Wall);
    map.insert((x,y - 1), Tile::Wall);
    map.insert((x,y + 1), Tile::Wall);
    map.insert((x - 1,y - 1), Tile::Key('1'));
    map.insert((x + 1,y - 1), Tile::Key('2'));
    map.insert((x - 1,y + 1), Tile::Key('3'));
    map.insert((x + 1,y + 1), Tile::Key('4'));

    let start_positions = vec![(x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)];
    let mut key_distance_map = build_key_distance_map(&map, &start_positions);

    let mut dp_map = HashMap::new();
    let result = solve2(&mut key_distance_map, &mut dp_map, &['1', '2', '3', '4'], HashSet::new(), keys_needed);
    println!("Part 2: {}", result);
}

fn build_map(s: String) -> (HashMap<Coord, Tile>, Coord) {
    use Tile::*;

    let mut m = HashMap::new();
    let mut player_coord = (0,0);
    for (row, s) in s.lines().enumerate() {
        for (col, c) in s.chars().enumerate() {
            let coord = (row as u32, col as u32);
            match c {
                '#' => { m.insert(coord, Wall); },
                '.' => { m.insert(coord, Floor); },
                '@' => {
                    m.insert(coord, Floor);
                    player_coord = coord;
                }
                c if c.is_ascii_lowercase() => { m.insert(coord, Key(c)); },
                c if c.is_ascii_uppercase() => { m.insert(coord, Door(c)); },
                _ => panic!("Read unexpected char '{}'", c)
            }
        }
    }

    (m, player_coord)
}

/// Return Map of kind (key a, key b) -> (distance, keys needed on the way)
/// Also adds distances from all `start_pos` to all other (alphabetic) keys
fn build_key_distance_map(map: &HashMap<Coord, Tile>, start_pos: &[Coord]) -> HashMap<(char, char), (u32, Vec<char>)> {
    let keys = map
        .iter()
        .filter_map(|(coord, tile)| {
            match tile {
                Tile::Key(c) if c.is_ascii_alphabetic() => Some((*coord, *c)),
                _ => None
            }
        })
        .collect::<Vec<(Coord, char)>>();

    let mut distance_map  = HashMap::new();

    for key1 in keys.iter() {
        for key2 in keys.iter() {
            if let Some((distance, needed_keys)) = distance_between(map, key1.0, key2.0) {
                distance_map.insert((key1.1, key2.1), (distance, needed_keys));
            }
        }
    }

    for (idx, pos) in start_pos.iter().enumerate() {
        for key1 in keys.iter() {
            if let Some((distance, needed_keys)) = distance_between(map, *pos, key1.0) {
                let start_key_name = char::from_digit(idx as u32 + 1, 10).unwrap();
                distance_map.insert((start_key_name, key1.1), (distance, needed_keys));
            }
        }
    }

    distance_map
}

/// Return (distance, keys needed on the way from a to b)
fn distance_between(map: &HashMap<Coord, Tile>, a: Coord, b: Coord) -> Option<(u32, Vec<char>)> {
    let mut seen = HashSet::new();
    let mut queue = VecDeque::from([(a, 0, vec![])]);
    'queue_loop: while !queue.is_empty() {
        let (pos, distance, mut keys) = queue.pop_front().unwrap();

        if pos == b {
            return Some((distance, keys));
        }

        if seen.contains(&pos) {
            continue 'queue_loop;
        }

        seen.insert(pos);

        let tile = map.get(&pos);
        if let Some(Tile::Door(c)) = tile {
            keys.push(c.to_ascii_lowercase());
        } else if let Some(Tile::Wall) = tile {
            continue 'queue_loop;
        }

        let neighbours = neighbours(pos);
        for neighbour_pos in neighbours {
            queue.push_back((neighbour_pos, distance + 1, keys.clone()));
        }
    }

    None
    // panic!("Did not find coord '{:?}' starting from '{:?}'", b, a);
}

fn solve1(
    key_map: &mut HashMap<(char,char), (u32, Vec<char>)>,
    dp_map: &mut HashMap<char, Vec<(HashSet<char>, u32)>>,
    start_key: char,
    keys_in_possession: HashSet<char>,
    keys_needed: HashSet<char>,
) -> u32 {
    {
        let dp_vec = dp_map.entry(start_key).or_default();

        let maybe_distance = dp_vec.iter().find_map(|(keys_in_possession_dp, distance)| {
            if keys_in_possession_dp.eq(&keys_in_possession) {
                Some(distance)
            } else {
                None
            }
        });

        if let Some(distance) = maybe_distance {
            return *distance;
        }
    }

    // Vec<('key', distance)>
    let next_stops = key_map
        .iter()
        .filter(|value| {
            value.0.0 == start_key
        })
        .filter(|value| {
            !keys_in_possession.contains(&value.0.1)
        })
        .filter(|value| {
            (*value.1).1.iter().all(|key| keys_in_possession.contains(key))
        })
        .map(|value| (value.0.1, value.1.0))
        .collect::<Vec<(char, u32)>>();

    let mut min_distance = None;
    
    for (key, distance) in next_stops {
        let remaining_keys_needed = {
            let mut keys_needed = keys_needed.clone();
            keys_needed.remove(&key);
            keys_needed
        };
        let remaining_keys_in_possession = {
            let mut keys_in_possession = keys_in_possession.clone();
            keys_in_possession.insert(key);
            keys_in_possession
        };
        let other_distances =
            solve1(
                key_map,
                dp_map,
                key,
                remaining_keys_in_possession,
                remaining_keys_needed);

        let d = distance + other_distances;
        min_distance = match min_distance {
            None => Some(d),
            Some(d2) => Some(d.min(d2))
        }
    }

    let min_distance = min_distance.unwrap_or(0);


    let dp_vec = dp_map.entry(start_key).or_default();
    if !dp_vec.iter().any(|(keys_in_possession_dp, _)| keys_in_possession.eq(keys_in_possession_dp)) {
        dp_vec.push((keys_in_possession, min_distance));
    }

    min_distance
}

fn solve2(
    key_map: &mut HashMap<(char,char), (u32, Vec<char>)>,
    dp_map: &mut HashMap<String, Vec<(HashSet<char>, u32)>>,
    start_keys: &[char],
    keys_in_possession: HashSet<char>,
    keys_needed: HashSet<char>,
) -> u32 {
    let start_key = start_keys[0];

    let current_positions = {
        let mut v = start_keys.to_vec();
        v.sort_unstable();
        v.iter().map(|c| c.to_string()).collect::<String>()
    };

    {
        let dp_vec = dp_map.entry(current_positions.clone()).or_default();

        let maybe_distance = dp_vec.iter().find_map(|(keys_in_possession_dp, distance)| {
            if keys_in_possession_dp.eq(&keys_in_possession) {
                Some(distance)
            } else {
                None
            }
        });

        if let Some(distance) = maybe_distance {
            return *distance;
        }
    }

    // Vec<('key', distance)>
    let next_stops = key_map
        .iter()
        .filter(|value| {
            value.0.0 == start_key
        })
        .filter(|value| {
            !keys_in_possession.contains(&value.0.1)
        })
        .filter(|value| {
            (*value.1).1.iter().all(|key| keys_in_possession.contains(key))
        })
        .map(|value| (value.0.1, value.1.0))
        .collect::<Vec<(char, u32)>>();

    if next_stops.is_empty() && !keys_needed.is_empty() {
        let mut new_start_keys = start_keys[1..].to_vec();
        new_start_keys.push(start_key);

        return solve2(key_map, dp_map, &new_start_keys, keys_in_possession, keys_needed);
    }

    let mut min_distance = None;

    for (key, distance) in next_stops {
        let remaining_keys_needed = {
            let mut keys_needed = keys_needed.clone();
            keys_needed.remove(&key);
            keys_needed
        };
        let remaining_keys_in_possession = {
            let mut keys_in_possession = keys_in_possession.clone();
            keys_in_possession.insert(key);
            keys_in_possession
        };

        let mut new_start_keys = start_keys[1..].to_vec();
        new_start_keys.push(key);

        let other_distances =
            solve2(
                key_map,
                dp_map,
                &new_start_keys,
                remaining_keys_in_possession,
                remaining_keys_needed);

        let d = distance + other_distances;
        min_distance = match min_distance {
            None => Some(d),
            Some(d2) => Some(d.min(d2))
        }
    }

    let min_distance = min_distance.unwrap_or(0);

    let dp_vec = dp_map.entry(current_positions).or_default();
    if !dp_vec.iter().any(|(keys_in_possession_dp, _)| keys_in_possession.eq(keys_in_possession_dp)) {
        dp_vec.push((keys_in_possession, min_distance));
    }

    min_distance
}
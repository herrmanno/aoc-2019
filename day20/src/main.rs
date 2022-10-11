use std::{collections::{HashMap, HashSet, BinaryHeap}, cmp::Reverse};

type Coord = (i32, i32);
type Maze = HashMap<Coord, Tile>;
type PortalMap = HashMap<Coord, Coord>;

#[derive(Debug, Clone)]
enum Tile {
    Floor,
    Wall,
    Portal(String)
}

impl TryFrom<char> for Tile {
    type Error = String;
    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            '.' => Ok(Tile::Floor),
            '#' => Ok(Tile::Wall),
            c if c.is_ascii_alphabetic() => Ok(Tile::Portal(format!("{}", c))),
            _ => Err(format!("Read unidentifiable map character '{}'", c))
        }
    }
}

impl From<&Tile> for char {
    fn from(tile: &Tile) -> char {
        match tile {
            Tile::Floor => '.',
            Tile::Wall => '#',
            Tile::Portal(s) => s.chars().next().unwrap(),
        }
    }
}

fn main() {
    let path = std::env::args().collect::<Vec<String>>().get(1).cloned().unwrap();
    let (map, portal_map, start_coord, final_coord) = prepare(&path);

    let shortest_path = find_shortest_path(&map, &portal_map, start_coord, final_coord);
    println!("Part 1: {}", shortest_path);

    let shortest_path_recursive = find_shortest_path_recursive(&map, &portal_map, start_coord, final_coord);
    println!("Part 2: {}", shortest_path_recursive);
}

fn prepare(path: &str) -> (Maze, PortalMap, Coord, Coord) {
    let map = read_input(path);
    let portal_map = get_portal_mapping(&map);
    let start_coord = get_start_position(&map);
    let final_coord = get_final_position(&map);

    (map, portal_map, start_coord, final_coord)
}

fn read_input(path: &str) -> Maze {
    let input = std::fs::read_to_string(path).unwrap();
    let mut map = HashMap::new();

    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if let Ok(tile) = Tile::try_from(c) {
                map.insert((y as i32, x as i32), tile);
            }
        }
    }

    // combine portals
    let portal_coords = map.iter()
        .filter_map(|(coord,tile)| {
            if matches!(tile, Tile::Portal(_)) {
                Some(coord)
            } else {
                None
            }
        })
        .cloned()
        .collect::<Vec<Coord>>();

    for coord in portal_coords {
        let (y,x) = coord;
        if let Some(Tile::Portal(name1)) = map.get(&coord).cloned() {
            if let Some(Tile::Portal(name2)) = map.get(&(y - 1, x)).cloned() {
                if map.contains_key(&(y - 2, x)) {
                    map.remove(&(y, x));
                    map.insert((y - 1, x), Tile::Portal(format!("{}{}", name2, name1)));
                } else {
                    map.remove(&(y - 1, x));
                    map.insert((y, x), Tile::Portal(format!("{}{}", name2, name1)));
                }
            }
            else if let Some(Tile::Portal(name2)) = map.get(&(y + 1, x)).cloned() {
                if map.contains_key(&(y + 2, x)) {
                    map.remove(&(y, x));
                    map.insert((y + 1, x), Tile::Portal(format!("{}{}", name1, name2)));
                } else {
                    map.remove(&(y + 1, x));
                    map.insert((y, x), Tile::Portal(format!("{}{}", name1, name2)));
                }

            }
            else if let Some(Tile::Portal(name2)) = map.get(&(y, x - 1)).cloned() {
                if map.contains_key(&(y, x - 2)) {
                    map.remove(&(y, x));
                    map.insert((y, x - 1), Tile::Portal(format!("{}{}", name2, name1)));
                } else {
                    map.remove(&(y, x - 1));
                    map.insert((y, x), Tile::Portal(format!("{}{}", name2, name1)));
                }
            }
            else if let Some(Tile::Portal(name2)) = map.get(&(y, x + 1)).cloned() {
                if map.contains_key(&(y, x + 2)) {
                    map.remove(&(y, x));
                    map.insert((y, x + 1), Tile::Portal(format!("{}{}", name1, name2)));
                } else {
                    map.remove(&(y, x + 1));
                    map.insert((y,x), Tile::Portal(format!("{}{}", name1, name2)));
                }
            }
        }
    }

    map
}

fn get_start_position(map: &Maze) -> Coord {
    let start_portal = map.iter()
        .find_map(|(coord, tile)| {
            match tile {
                Tile::Portal(name) if name == "AA" => Some(coord),
                _ => None
            }
        })
        .cloned()
        .unwrap();

    neighbours(start_portal)
        .into_iter()
        .find(|coord| {
            matches!(map.get(coord), Some(Tile::Floor))
        })
        .unwrap()
}

fn get_final_position(map: &Maze) -> Coord {
    let final_portal = map.iter()
        .find_map(|(coord, tile)| {
            match tile {
                Tile::Portal(name) if name == "ZZ" => Some(coord),
                _ => None
            }
        })
        .cloned()
        .unwrap();

    neighbours(final_portal)
        .into_iter()
        .find(|coord| {
            matches!(map.get(coord), Some(Tile::Floor))
        })
        .unwrap()
}

fn get_portal_mapping(map: &Maze) -> PortalMap {
    let mut help_map: HashMap<String, Vec<Coord>> = HashMap::new();
    let mut portal_map = HashMap::new();

    map.iter()
        .filter_map(|(coord, tile)| {
            match tile {
                Tile::Portal(name) => Some((coord, name)),
                _ => None
            }
        })
        .for_each(|(coord, name)| {
            help_map
                .entry(name.clone())
                .and_modify(|v| v.push(*coord))
                .or_insert_with(|| vec![*coord]);
        });
    
    for (_, coords) in help_map {
        if let [a,b] = coords[..] {
            portal_map.insert(a, b);
            portal_map.insert(b, a);
        }
    }

    portal_map
}

fn find_shortest_path(map: &Maze, portal_map: &PortalMap, start: Coord, end: Coord) -> u32 {
    let mut visited = HashSet::new();
    let mut queue = BinaryHeap::from(vec![Reverse((0, start))]);

    while let Some(Reverse((distance, coord))) = queue.pop() {
        if coord == end {
            return distance;
        }

        if visited.contains(&coord) {
            continue;
        }
        visited.insert(coord);

        for neighbour in neighbours(coord) {
            match map.get(&neighbour) {
                Some(Tile::Floor) => {
                    queue.push(Reverse((distance + 1, neighbour)));
                }
                Some(Tile::Portal(_)) => {
                    if let Some(&portal_out) = portal_map.get(&neighbour) {
                        let &new_coord = neighbours(portal_out)
                            .iter()
                            .find(|coord| {
                                matches!(map.get(coord), Some(Tile::Floor))
                            }).unwrap();
                        queue.push(Reverse((distance + 1, new_coord)));
                    }
                }
                _ => { continue; }
            }
        }
    }

    0
}

fn find_shortest_path_recursive(map: &Maze, portal_map: &PortalMap, start: Coord, end: Coord) -> u32 {
    let min_y = map.keys().map(|c| c.0).min().unwrap();
    let min_x = map.keys().map(|c| c.1).min().unwrap();
    let max_y = map.keys().map(|c| c.0).max().unwrap();
    let max_x = map.keys().map(|c| c.1).max().unwrap();

    let is_outer_portal  = move |coord: Coord| {
        coord.0 == min_y || coord.0 == max_y || coord.1 == min_x || coord.1 == max_x
    };

    let mut visited = HashSet::new();
    let mut queue = BinaryHeap::from(vec![Reverse((0, 0, start))]);

    while let Some(Reverse((distance, level, coord))) = queue.pop() {
        if level == 0 && coord == end {
            return distance;
        }

        if visited.contains(&(coord, level)) {
            continue;
        }
        visited.insert((coord, level));

        for neighbour in neighbours(coord) {
            match map.get(&neighbour) {
                Some(Tile::Floor) => {
                    queue.push(Reverse((distance + 1, level, neighbour)));
                }
                Some(Tile::Portal(_)) => {
                    if let Some(&portal_out) = portal_map.get(&neighbour) {
                        let &new_coord = neighbours(portal_out)
                            .iter()
                            .find(|coord| {
                                matches!(map.get(coord), Some(Tile::Floor))
                            }).unwrap();
                        
                        if is_outer_portal(neighbour) {
                            if level > 0 {
                                queue.push(Reverse((distance + 1, level - 1, new_coord)));
                            }
                        } else {
                            queue.push(Reverse((distance + 1, level + 1, new_coord)));
                        }
                    }
                }
                _ => { continue; }
            }
        }
    }

    0
}

fn neighbours(coord: Coord) -> [Coord; 4] {
    let (y,x) = coord;
    [(y, x + 1), (y, x - 1), (y + 1, x), (y - 1, x)]
}

#[allow(dead_code)]
fn print_map(map: &Maze) {
    let max_y = map.keys().map(|coord| coord.0).max().unwrap();
    let max_x = map.keys().map(|coord| coord.1).max().unwrap();

    for y in 0..=max_y {
        for x in 0..=max_x {
            let c = map.get(&(y,x)).map(char::from).unwrap_or(' ');
            print!("{}", c);
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let (map, portal_map, start, end) = prepare("test1.txt");
        let shortest_path = find_shortest_path(&map, &portal_map, start, end);
        assert_eq!(23, shortest_path);

        let (map, portal_map, start, end) = prepare("test2.txt");
        let shortest_path = find_shortest_path(&map, &portal_map, start, end);
        assert_eq!(58, shortest_path);
    }

    #[test]
    fn test_part2() {
        let (map, portal_map, start, end) = prepare("test3.txt");
        let shortest_path = find_shortest_path_recursive(&map, &portal_map, start, end);
        assert_eq!(396, shortest_path);
    }

}
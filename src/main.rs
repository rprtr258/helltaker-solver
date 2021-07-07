use std::fmt::{Display, Formatter, Result};
use std::collections::{VecDeque, HashMap};
use std::ops::{Index, IndexMut};

#[derive(Clone, PartialEq, Eq, Hash, Copy, Debug)]
enum LaserSource {Down, Right}

#[derive(Clone, PartialEq, Eq, Hash, Copy, Debug)]
enum Cell {Wall, Laser, LeverOff, LeverOn, Cube, Space, Player, LaserSource(LaserSource)}

impl From<&Cell> for char {
    fn from(c: &Cell) -> Self {
        use Cell::*;
        use crate::LaserSource::*;
        match c {
            Wall => 'X',
            Laser => '-',
            LeverOff => 'L',
            LeverOn => 'l',
            Cube => 'O',
            LaserSource(Down) => 'v',
            LaserSource(Right) => 'r',
            Space => ' ',
            Player => 'p',
        }
    }
}

type MapRepr = Vec<Cell>;

#[derive(Clone, PartialEq, Eq, Hash)]
struct Map {
    mp: MapRepr,
    laser_poss: Vec<(usize, (isize, isize, isize))>,
    width: usize,
    height: usize,
}

impl Map {
    // TODO: change to in_map?
    fn not_in_map(&self, (x, y): (isize, isize)) -> bool {
        !((0..self.width as isize).contains(&x) && (0..self.height as isize).contains(&y))
    }

    fn draw_lasers(&self, mp: &mut MapRepr) {
        let mut to_update: Vec<usize> = Vec::new();
        for (i, (dx, dy, d)) in self.laser_poss.iter() {
            let (mut kx, mut ky, mut k) = ((i % self.width) as isize, (i / self.width) as isize, *i as isize);
            while !self.not_in_map((kx, ky)) {
                match mp[ky as usize * self.width + kx as usize] {
                    Cell::LaserSource(_) | Cell::Laser => {}
                    Cell::Space => {to_update.push(k as usize)}
                    _ => {break;}
                };
                k += d;
                kx += dx;
                ky += dy;
            }
        }
        to_update.iter().for_each(|k| mp[*k] = Cell::Laser)
    }

    fn bfs(&self, max_turns: u32, v_start: (isize, isize)) -> Option<Vec<MapRepr>> {
        let mut visited = HashMap::new(); // from
        let mut queue: VecDeque<(MapRepr, (usize, isize, isize), MapRepr, u32)> = VecDeque::new();
        {
            let mp_vec = {
                let mut tmp = self.mp.clone();
                self.draw_lasers(&mut tmp);
                tmp
            };
            queue.push_back((mp_vec.clone(), (v_start.1 as usize * self.width + v_start.0 as usize, v_start.0, v_start.1), mp_vec, 0));
        }
        while !queue.is_empty() {
            let (strmap, (vi, v_x, v_y), p, sz) = queue.pop_front().unwrap();
            if visited.contains_key(&strmap) {
                continue
            }
            if !strmap.contains(&Cell::LeverOff) {
                println!("Found solution in {} steps", sz);
                visited.insert(strmap.clone(), p);
                let mut res = Vec::new();
                let mut cur = &strmap;
                while *cur != visited[cur] {
                    res.push(cur.clone());
                    cur = &visited[cur];
                }
                return Some(res)
            }
            visited.insert(strmap.clone(), p.clone());
            for (dx, dy, dir) in [(0, 1), (0, -1), (1, 0), (-1, 0)].iter()
                .map(|(x, y)| (x, y, y * self.width as isize + x)) {
                let (to_x, to_y) = (v_x + dx, v_y + dy);
                if self.not_in_map((to_x, to_y)) {
                    continue;
                }
                let to = (vi as isize + dir) as usize;
                let to_obj = strmap[to];
                // can't go out of map and into laser or turned on lever
                match to_obj {
                    Cell::Wall | Cell::Laser | Cell::LeverOn => continue,
                    _ => {}
                }
                let (new_strmap, new_pos) = match to_obj {
                    Cell::Cube => {
                        let after_to = {
                            let after_to = to as isize + dir;
                            if self.not_in_map((to_x + dx, to_y + dy)) {
                                continue;
                            }
                            after_to as usize
                        };
                        // only can move box into space or laser
                        match strmap[after_to] {
                            Cell::Space | Cell::Laser => {},
                            _ => continue
                        }
                        let strmap_new: MapRepr = {
                            let mut tmp = strmap.clone();
                            for c in tmp.iter_mut() {
                                *c = match c {
                                    Cell::Laser => Cell::Space,
                                    _ => *c
                                };
                            }
                            replace(&mut tmp, to, Cell::Space, after_to, Cell::Cube);
                            self.draw_lasers(&mut tmp);
                            tmp
                        };
                        (strmap_new, (vi, v_x, v_y))
                    }
                    Cell::Space => {
                        let mut strmap_new = strmap.clone();
                        replace(&mut strmap_new, vi, Cell::Space, to, Cell::Player);
                        (strmap_new, (to, to_x, to_y))
                    }
                    Cell::LeverOff => {
                        let mut strmap_new = strmap.clone();
                        strmap_new[to] = Cell::LeverOn;
                        (strmap_new, (vi, v_x, v_y))
                    }
                    _ => unreachable!()
                };
                if sz + 1 <= max_turns {
                    queue.push_back((new_strmap, new_pos, strmap.clone(), sz + 1));
                }
            }
        }
        None
    }
}

fn replace(v: &mut MapRepr, p1: usize, c1: Cell, p2: usize, c2: Cell) {
    v[p1] = c1;
    v[p2] = c2;
}

impl From<&[(Cell, &[(usize, usize)])]> for Map {
    fn from(objects: &[(Cell, &[(usize, usize)])]) -> Map {
        let (mx, my): (usize, usize) = objects.iter()
            .flat_map(|(_, poss)| poss.iter().cloned())
            .map(|(x, y)| (x + 1, y + 1))
            .reduce(|(mx, my), (x, y)| (mx.max(x), my.max(y)))
            .unwrap();
        let laser_poss = objects.iter()
            .filter_map(|(c, ps)| match c {
                Cell::LaserSource(x) => Some((x, ps)),
                _ => None
            })
            .flat_map(|(c, ps)| ps.iter()
                .map(|(x, y)| y * mx + x)
                .map(move |p| (p, match c {
                    LaserSource::Down => (0, -1, -(mx as isize)),
                    LaserSource::Right => (1, 0, 1),
                })))
            .collect();
        let mut res = Map {
            mp: vec![Cell::Space; mx * my],
            width: mx,
            height: my,
            laser_poss: laser_poss
        };
        objects.iter()
            .for_each(|(obj, poss)|
                poss.iter().for_each(|p| {
                    res[*p] = *obj
                })
        );
        res
    }
}

impl Index<(usize, usize)> for Map {
    type Output = Cell;
    fn index(&self, (x, y): (usize, usize)) -> &Cell {
        &self.mp[y * self.width + x]
    }
}

impl IndexMut<(usize, usize)> for Map {
    fn index_mut(&mut self, (x, y): (usize, usize)) -> &mut Cell {
        &mut self.mp[y * self.width + x]
    }
}

impl Display for Map {
    fn fmt(&self, fmt: &mut Formatter) -> Result {
        let mut tmp = self.mp.clone();
        self.draw_lasers(&mut tmp);
        fmt.write_str(&tmp.iter()
            .map(char::from)
            .collect::<Vec<_>>()
            .chunks(self.width)
            .map(|x| x.iter().fold(String::new(), |mut acc, b| {acc.push(*b); acc}))
            .rev()
            .fold(String::new(), |mut acc, b| {acc.push_str(&b); acc.push('\n'); acc}))
    }
}

fn main() {
    let player_pos = (7, 0);
    let max_turns = 49;
    let map: Map = Map::from(&[
        (Cell::Cube, &[(1, 3), (5, 2), (7, 1), (7, 2), (7, 3)][..]),
        (Cell::Wall, &[(0, 1), (1, 1), (2, 0), (3, 0), (4, 0), (5, 0), (8, 4), (7, 4), (7, 5), (1, 5), (0, 4)][..]),
        (Cell::LaserSource(LaserSource::Down), &[(2, 5), (3, 5), (6, 5)][..]),
        (Cell::LaserSource(LaserSource::Right), &[(3, 4)][..]),
        (Cell::LeverOff, &[(1, 4), (5, 5)][..]),
        (Cell::Player, &[player_pos][..]),
    ][..]);

    println!("{}", map);
    let solution = {map.bfs(max_turns, (player_pos.0 as isize, player_pos.1 as isize)).unwrap()};
    solution.iter().rev().for_each(|state|
        println!("{}", Map { mp: state.clone(), width: map.width, height: map.height, laser_poss: map.laser_poss.clone() })
    );
}

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
struct MapMeta {
    laser_poss: Vec<(usize, (isize, isize, isize))>,
    width: usize,
    height: usize,
    player_x: usize,
    player_y: usize,
}

impl MapMeta {
    fn from(objects: &[(Cell, &[(usize, usize)])], player_pos: (usize, usize)) -> (MapMeta, MapRepr) {
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
        let mut mp = vec![Cell::Space; mx * my];
        let res = MapMeta {
            width: mx,
            height: my,
            laser_poss: laser_poss,
            player_x: player_pos.0,
            player_y: player_pos.1,
        };
        objects.iter()
            .for_each(|(obj, poss)| poss.iter()
                .map(|p| res.from_pair(p))
                .for_each(|p| {
                    mp[p] = *obj
                })
        );
        mp[res.from_pair(&player_pos)] = Cell::Player;
        (res, mp)
    }

    fn from_pair<I: Copy>(&self, (x, y): &(I, I)) -> usize where usize: std::convert::TryFrom<I> {
        use std::convert::TryFrom;
        let (ix, iy) = (usize::try_from(*x), usize::try_from(*y));
        match ix {
            Ok(ix) => match iy {
                Ok(iy) => self.width * iy + ix,
                _ => unreachable!()
            },
            _ => unreachable!()
        }
    }

    fn fmt(&self, mp: &MapRepr) -> String {
        let mut tmp = mp.clone();
        self.draw_lasers(&mut tmp);
        tmp.iter()
            .map(char::from)
            .collect::<Vec<_>>()
            .chunks(self.width)
            .map(|x| x.iter().fold(String::new(), |mut acc, b| {acc.push(*b); acc}))
            .rev()
            .fold(String::new(), |mut acc, b| {acc.push_str(&b); acc.push('\n'); acc})
    }

    // TODO: change to in_map?
    fn not_in_map(&self, (x, y): (isize, isize)) -> bool {
        !((0..self.width as isize).contains(&x) && (0..self.height as isize).contains(&y))
    }

    fn is_player_dead(&self, mp: &MapRepr, player_pos: &(isize, isize)) -> bool {
        for (i, (dx, dy, d)) in self.laser_poss.iter() {
            let (mut kx, mut ky, mut k) = ((i % self.width) as isize, (i / self.width) as isize, *i as isize);
            match *player_pos {
                (x, y) if kx != x && ky != y => continue,
                _ => while !self.not_in_map((kx, ky)) {
                    match mp[k as usize] {
                        Cell::LaserSource(_) | Cell::Laser | Cell::Space => {}
                        Cell::Player => return true,
                        _ => break,
                    };
                    k += d;
                    kx += dx;
                    ky += dy;
                }
            }
        }
        false
    }

    fn draw_lasers(&self, mp: &mut MapRepr) {
        for (i, (dx, dy, d)) in self.laser_poss.iter() {
            let (k0, times) = {
                let (kx, ky) = ((i % self.width) as isize, (i / self.width) as isize);
                (*i as isize, match *dx {
                    0 => match *dy {
                        _dy if _dy < 0 => ky,
                        _dy => self.height as isize - ky,
                    }
                    _dx if _dx < 0 => kx,
                    _dx => self.width as isize - kx,
                } + 1)
            };
            for k in (0..times).into_iter()
                .map(|x| k0 + d * x)
                .map(|k| k as usize) {
                match unsafe {mp.get_unchecked(k)} {
                    Cell::LaserSource(_) | Cell::Laser => {}
                    Cell::Space => {unsafe {*mp.get_unchecked_mut(k) = Cell::Laser}}
                    _ => {break;}
                };
            }
        }
    }

    fn bfs(&self, max_turns: u32, v_start: (isize, isize), mp: MapRepr) -> Option<Vec<MapRepr>> {
        let mut from = std::collections::HashMap::new();
        let mut queue = std::collections::VecDeque::new();
        queue.push_back((mp.clone(), (self.from_pair(&v_start), v_start.0, v_start.1), mp, 0));
        while !queue.is_empty() {
            let (strmap, (vi, v_x, v_y), p, sz) = queue.pop_front().unwrap();
            if from.contains_key(&strmap) || self.is_player_dead(&strmap, &(v_x, v_y)) {
                continue
            }
            if !strmap.contains(&Cell::LeverOff) {
                from.insert(strmap.clone(), p);
                let mut res = Vec::new();
                let mut cur = &strmap;
                while *cur != from[cur] {
                    res.push(cur.clone());
                    cur = &from[cur];
                }
                return Some(res)
            }
            from.insert(strmap.clone(), p.clone());
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
                            tmp[to] = Cell::Space;
                            tmp[after_to] = Cell::Cube;
                            tmp
                        };
                        (strmap_new, (vi, v_x, v_y))
                    }
                    Cell::Space => {
                        let mut strmap_new = strmap.clone();
                        strmap_new[vi] = Cell::Space;
                        strmap_new[to] = Cell::Player;
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

// #[bench]
// fn benchmark_bfs(b: &mut test::Bencher) {
// fn benchmark_bfs() {
//     let player_pos = (7, 0);
//     let max_turns = 49;
//     let (map_meta, map_repr) = MapMeta::from(&[
//         (Cell::Cube, &[(1, 3), (5, 2), (7, 1), (7, 2), (7, 3)][..]),
//         (Cell::Wall, &[(0, 1), (1, 1), (2, 0), (3, 0), (4, 0), (5, 0), (8, 4), (7, 4), (7, 5), (1, 5), (0, 4)][..]),
//         (Cell::LaserSource(LaserSource::Down), &[(2, 5), (3, 5), (6, 5)][..]),
//         (Cell::LaserSource(LaserSource::Right), &[(3, 4)][..]),
//         (Cell::LeverOff, &[(1, 4), (5, 5)][..]),
//     ][..], player_pos);
//     (0..30).for_each(|c| {
//         map_meta.bfs(max_turns, (player_pos.0 as isize, player_pos.1 as isize), map_repr);
//     });
// }
// fn run_benchmark(d: MapMeta, a: u32, b: (isize, isize), c: MapRepr) -> Vec<Vec<MapRepr>> {
//     let n = 30;
//     let mut res = Vec::with_capacity(n);
//     let mut times = Vec::with_capacity(n);
//     for _ in 0..n {
//         let start = std::time::Instant::now();
//         res.push(d.bfs(a, b, c.clone()).unwrap());
//         times.push(start.elapsed().as_millis());
//     }
//     println!("{}", times.into_iter().min().unwrap());
//     res
// }

fn main() {
    let player_pos = (7, 0);
    let max_turns = 49;
    let (map_meta, map_repr) = MapMeta::from(&[
        (Cell::Cube, &[(1, 3), (5, 2), (7, 1), (7, 2), (7, 3)][..]),
        (Cell::Wall, &[(0, 1), (1, 1), (2, 0), (3, 0), (4, 0), (5, 0), (8, 4), (7, 4), (7, 5), (1, 5), (0, 4)][..]),
        (Cell::LaserSource(LaserSource::Down), &[(2, 5), (3, 5), (6, 5)][..]),
        (Cell::LaserSource(LaserSource::Right), &[(3, 4)][..]),
        (Cell::LeverOff, &[(1, 4), (5, 5)][..]),
    ][..], player_pos);
    // run_benchmark(map_meta.clone(), max_turns, (player_pos.0 as isize, player_pos.1 as isize), map_repr.clone());
    println!("Starting position:\n{}", map_meta.fmt(&map_repr));
    match map_meta.bfs(max_turns, (player_pos.0 as isize, player_pos.1 as isize), map_repr) {
        Some(solution) => {
            println!("Found solution in {} steps", solution.len());
            solution.iter().rev().map(|x| map_meta.fmt(x)).for_each(|state| {
                println!("=======================");
                print!("{}", state);
            })
        }
        None => println!("Solution was not found :("),
    }
}

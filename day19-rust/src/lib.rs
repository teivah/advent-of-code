extern crate core;

use std::cmp;
use std::collections::hash_map::{DefaultHasher, Entry};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ops::Index;

pub fn fn1(input: &str, minute: i32) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    let mut sum = 0;
    for line in lines.iter() {
        let blueprint = to_blueprints(line);

        let mut cache: HashMap<i32, HashMap<u64, i32>> = HashMap::new();
        let v = best(
            &mut cache,
            &blueprint,
            State {
                nb_ore: 0,
                nb_clay: 0,
                nb_obsidian: 0,
                nb_geode: 0,
                nb_ore_robot: 1,
                nb_clay_robot: 0,
                nb_obsidian_robot: 0,
                nb_geode_robot: 0,
            },
            minute,
        );
        println!("{} {}", blueprint.id, v);
        sum += blueprint.id * v;
    }

    sum
}

fn get_cache(cache: &mut HashMap<i32, HashMap<u64, i32>>, left: i32, state: u64) -> (i32, bool) {
    if let Entry::Occupied(v) = cache.entry(left) {
        let m = v.into_mut();
        if let Entry::Occupied(v) = m.entry(state) {
            let m = v.into_mut();
            return (*m, true);
        }
    }

    (0, false)
}

fn add_cache(cache: &mut HashMap<i32, HashMap<u64, i32>>, left: i32, state: u64, best: i32) {
    let m = cache.entry(left).or_insert(HashMap::new());
    m.entry(state).or_insert(best);
}

fn best(
    cache: &mut HashMap<i32, HashMap<u64, i32>>,
    blueprint: &Blueprint,
    state: State,
    left: i32,
) -> i32 {
    if left == 0 {
        return state.nb_geode;
    }

    let mut h = DefaultHasher::new();
    state.hash(&mut h);
    let hk = h.finish();
    let (v, exists) = get_cache(cache, left, h.finish());
    if exists {
        return v;
    }

    let mut v = 0;

    if let Some(new_state) = new_geode_robot(blueprint, &state) {
        let s = new_state.combine_geode();
        let v = best(cache, blueprint, s, left - 1);
        add_cache(cache, left, hk, v);
        return v;
    }

    if let Some(new_state) = new_obsidian_robot(blueprint, &state) {
        let s = new_state.combine_obsidian();
        v = best(cache, blueprint, s, left - 1);

        if left >= 5 {
            add_cache(cache, left, hk, v);
            return v;
        }
    }

    if let Some(new_state) = new_clay_robot(blueprint, &state) {
        let s = new_state.combine_clay();
        v = cmp::max(v, best(cache, blueprint, s, left - 1));
    }

    if let Some(new_state) = new_ore_robot(blueprint, &state) {
        let s = new_state.combine_ore();
        v = cmp::max(v, best(cache, blueprint, s, left - 1));
    }

    let s = state.combine();
    v = cmp::max(v, best(cache, blueprint, s, left - 1));
    add_cache(cache, left, h.finish(), v);

    v
}

fn new_ore_robot(blueprint: &Blueprint, state: &State) -> Option<State> {
    if blueprint.ore_ore_cost <= state.nb_ore {
        let mut s = state.clone();
        s.nb_ore -= blueprint.ore_ore_cost;
        s.nb_ore_robot += 1;
        return Some(s);
    }
    return None;
}

fn new_clay_robot(blueprint: &Blueprint, state: &State) -> Option<State> {
    if blueprint.clay_ore_cost <= state.nb_ore {
        let mut s = state.clone();
        s.nb_ore -= blueprint.clay_ore_cost;
        s.nb_clay_robot += 1;
        return Some(s);
    }
    return None;
}

fn new_obsidian_robot(blueprint: &Blueprint, state: &State) -> Option<State> {
    if blueprint.obsidian_ore_cost <= state.nb_ore && blueprint.obsidian_clay_cost <= state.nb_clay
    {
        let mut s = state.clone();
        s.nb_ore -= blueprint.obsidian_ore_cost;
        s.nb_clay -= blueprint.obsidian_clay_cost;
        s.nb_obsidian_robot += 1;
        return Some(s);
    }
    return None;
}

fn new_geode_robot(blueprint: &Blueprint, state: &State) -> Option<State> {
    if blueprint.geode_ore_cost <= state.nb_ore
        && blueprint.geode_obsidian_cost <= state.nb_obsidian
    {
        let mut s = state.clone();
        s.nb_ore -= blueprint.geode_ore_cost;
        s.nb_obsidian -= blueprint.geode_obsidian_cost;
        s.nb_geode_robot += 1;
        return Some(s);
    }
    return None;
}

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
struct State {
    nb_ore: i32,
    nb_clay: i32,
    nb_obsidian: i32,
    nb_geode: i32,
    nb_ore_robot: i32,
    nb_clay_robot: i32,
    nb_obsidian_robot: i32,
    nb_geode_robot: i32,
}

impl State {
    fn combine(&self) -> State {
        State {
            nb_ore: self.nb_ore + self.nb_ore_robot,
            nb_clay: self.nb_clay + self.nb_clay_robot,
            nb_obsidian: self.nb_obsidian + self.nb_obsidian_robot,
            nb_geode: self.nb_geode + self.nb_geode_robot,
            nb_ore_robot: self.nb_ore_robot,
            nb_clay_robot: self.nb_clay_robot,
            nb_obsidian_robot: self.nb_obsidian_robot,
            nb_geode_robot: self.nb_geode_robot,
        }
    }

    fn combine_ore(&self) -> State {
        State {
            nb_ore: self.nb_ore + self.nb_ore_robot - 1,
            nb_clay: self.nb_clay + self.nb_clay_robot,
            nb_obsidian: self.nb_obsidian + self.nb_obsidian_robot,
            nb_geode: self.nb_geode + self.nb_geode_robot,
            nb_ore_robot: self.nb_ore_robot,
            nb_clay_robot: self.nb_clay_robot,
            nb_obsidian_robot: self.nb_obsidian_robot,
            nb_geode_robot: self.nb_geode_robot,
        }
    }

    fn combine_clay(&self) -> State {
        State {
            nb_ore: self.nb_ore + self.nb_ore_robot,
            nb_clay: self.nb_clay + self.nb_clay_robot - 1,
            nb_obsidian: self.nb_obsidian + self.nb_obsidian_robot,
            nb_geode: self.nb_geode + self.nb_geode_robot,
            nb_ore_robot: self.nb_ore_robot,
            nb_clay_robot: self.nb_clay_robot,
            nb_obsidian_robot: self.nb_obsidian_robot,
            nb_geode_robot: self.nb_geode_robot,
        }
    }

    fn combine_obsidian(&self) -> State {
        State {
            nb_ore: self.nb_ore + self.nb_ore_robot,
            nb_clay: self.nb_clay + self.nb_clay_robot,
            nb_obsidian: self.nb_obsidian + self.nb_obsidian_robot - 1,
            nb_geode: self.nb_geode + self.nb_geode_robot,
            nb_ore_robot: self.nb_ore_robot,
            nb_clay_robot: self.nb_clay_robot,
            nb_obsidian_robot: self.nb_obsidian_robot,
            nb_geode_robot: self.nb_geode_robot,
        }
    }

    fn combine_geode(&self) -> State {
        State {
            nb_ore: self.nb_ore + self.nb_ore_robot,
            nb_clay: self.nb_clay + self.nb_clay_robot,
            nb_obsidian: self.nb_obsidian + self.nb_obsidian_robot,
            nb_geode: self.nb_geode + self.nb_geode_robot - 1,
            nb_ore_robot: self.nb_ore_robot,
            nb_clay_robot: self.nb_clay_robot,
            nb_obsidian_robot: self.nb_obsidian_robot,
            nb_geode_robot: self.nb_geode_robot,
        }
    }
}

#[derive(Debug)]
struct Blueprint {
    id: i32,
    ore_ore_cost: i32,
    clay_ore_cost: i32,
    obsidian_ore_cost: i32,
    obsidian_clay_cost: i32,
    geode_ore_cost: i32,
    geode_obsidian_cost: i32,
}

fn to_blueprints(s: &str) -> Blueprint {
    let start = 10;
    let end = s.find(":").unwrap();
    let id = s[start..end].parse::<i32>().unwrap();

    let start = s.find("Each geode robot costs ").unwrap();
    let start = s[start..].find("ore and ").unwrap() + start + "ore and ".len();
    let end = s[start..].find(" ").unwrap();
    let geode_obsidian_cost = s[start..start + end].parse::<i32>().unwrap();

    Blueprint {
        id,
        ore_ore_cost: get_cost(s, "Each ore robot costs "),
        clay_ore_cost: get_cost(s, "Each clay robot costs "),
        obsidian_ore_cost: get_cost(s, "Each obsidian robot costs "),
        obsidian_clay_cost: get_cost(s, " ore and "),
        geode_ore_cost: get_cost(s, "Each geode robot costs "),
        geode_obsidian_cost,
    }
}

fn get_cost(s: &str, sep: &str) -> i32 {
    let start = s.find(sep).unwrap();
    let end = s[(start + sep.len())..].find(" ").unwrap();
    s[(start + sep.len()..(end + start + sep.len()))]
        .parse::<i32>()
        .unwrap()
}

pub fn fn2(input: &str) -> i32 {
    let lines: Vec<_> = input.lines().collect();

    1
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_fn1_unit_test() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str(), 20), 33);
    }

    #[test]
    fn test_fn1_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn1(s.as_str(), 24), 33);
    }

    #[test]
    fn test_fn1_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn1(s.as_str(), 24), 1147);
    }

    #[test]
    fn test_fn2_unit() {
        let s = fs::read_to_string("test.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1);
    }

    #[test]
    fn test_fn2_input() {
        let s = fs::read_to_string("input.txt").unwrap();
        assert_eq!(fn2(s.as_str()), 1);
    }
}

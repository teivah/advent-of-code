extern crate core;

use std::cmp;
use std::collections::hash_map::{DefaultHasher, Entry};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::ops::Index;

pub fn fn1(input: &str, minute: u64) -> u64 {
    let lines: Vec<_> = input.lines().collect();

    let mut sum = 0;
    for line in lines.iter() {
        let blueprint = to_blueprints(line);

        let mut cache: HashMap<_, _> = HashMap::new();
        let v = best(&mut cache, &blueprint, 0, 0, 0, 0, 1, 0, 0, 0, minute);
        println!("{} {}", blueprint.id, v);
        sum += blueprint.id * v;
    }

    sum
}

fn get_cache(cache: &HashMap<u64, u64>, k: u64) -> Option<u64> {
    if let Some(v) = cache.get(&k) {
        return Some(*v);
    }

    None
}

fn add_cache(cache: &mut HashMap<u64, u64>, k: u64, best: u64) {
    cache.insert(k, best);
}

fn best(
    cache: &mut HashMap<u64, u64>,
    blueprint: &Blueprint,
    nb_ore: u64,
    nb_clay: u64,
    nb_obsidian: u64,
    nb_geode: u64,
    nb_ore_robot: u64,
    nb_clay_robot: u64,
    nb_obsidian_robot: u64,
    nb_geode_robot: u64,
    left: u64,
) -> u64 {
    if left == 0 {
        return nb_geode;
    }

    let k: u64 = nb_geode_robot
        | nb_obsidian_robot << 8
        | nb_clay_robot << 16
        | nb_ore_robot << 24
        | nb_geode << 32
        | nb_obsidian << 38
        | nb_clay << 44
        | nb_ore << 50
        | left << 56;
    if let Some(v) = get_cache(cache, k) {
        return v;
    }

    if blueprint.geode_ore_cost <= nb_ore && blueprint.geode_obsidian_cost <= nb_obsidian {
        let v = best(
            cache,
            blueprint,
            nb_ore - blueprint.geode_ore_cost + nb_ore_robot,
            nb_clay + nb_clay_robot,
            nb_obsidian - blueprint.geode_obsidian_cost + nb_obsidian_robot,
            nb_geode + nb_geode_robot,
            nb_ore_robot,
            nb_clay_robot,
            nb_obsidian_robot,
            nb_geode_robot + 1,
            left - 1,
        );
        add_cache(cache, k, v);
        return v;
    }

    let mut v = 0;

    if blueprint.obsidian_ore_cost <= nb_ore && blueprint.obsidian_clay_cost <= nb_clay {
        let v = best(
            cache,
            blueprint,
            nb_ore - blueprint.obsidian_ore_cost + nb_ore_robot,
            nb_clay - blueprint.obsidian_clay_cost + nb_clay_robot,
            nb_obsidian + nb_obsidian_robot,
            nb_geode + nb_geode_robot,
            nb_ore_robot,
            nb_clay_robot,
            nb_obsidian_robot + 1,
            nb_geode_robot,
            left - 1,
        );
        if left >= 5 {
            add_cache(cache, k, v);
            return v;
        }
    }

    if blueprint.clay_ore_cost <= nb_ore {
        v = cmp::max(
            v,
            best(
                cache,
                blueprint,
                nb_ore - blueprint.clay_ore_cost + nb_ore_robot,
                nb_clay + nb_clay_robot,
                nb_obsidian + nb_obsidian_robot,
                nb_geode + nb_geode_robot,
                nb_ore_robot,
                nb_clay_robot + 1,
                nb_obsidian_robot,
                nb_geode_robot,
                left - 1,
            ),
        );
    }

    if blueprint.ore_ore_cost <= nb_ore {
        v = cmp::max(
            v,
            best(
                cache,
                blueprint,
                nb_ore - blueprint.ore_ore_cost + nb_ore_robot,
                nb_clay + nb_clay_robot,
                nb_obsidian + nb_obsidian_robot,
                nb_geode + nb_geode_robot,
                nb_ore_robot + 1,
                nb_clay_robot,
                nb_obsidian_robot,
                nb_geode_robot,
                left - 1,
            ),
        );
    }

    v = cmp::max(
        v,
        best(
            cache,
            blueprint,
            nb_ore + nb_ore_robot,
            nb_clay + nb_clay_robot,
            nb_obsidian + nb_obsidian_robot,
            nb_geode + nb_geode_robot,
            nb_ore_robot,
            nb_clay_robot,
            nb_obsidian_robot,
            nb_geode_robot,
            left - 1,
        ),
    );
    add_cache(cache, k, v);

    v
}

#[derive(PartialEq, Eq, Hash, Copy, Clone)]
struct CacheEntry {
    nb_ore: i32,
    nb_clay: i32,
    nb_obsidian: i32,
    nb_geode: i32,
    nb_ore_robot: i32,
    nb_clay_robot: i32,
    nb_obsidian_robot: i32,
    nb_geode_robot: i32,
}

#[derive(Debug)]
struct Blueprint {
    id: u64,
    ore_ore_cost: u64,
    clay_ore_cost: u64,
    obsidian_ore_cost: u64,
    obsidian_clay_cost: u64,
    geode_ore_cost: u64,
    geode_obsidian_cost: u64,
}

fn to_blueprints(s: &str) -> Blueprint {
    let start = 10;
    let end = s.find(":").unwrap();
    let id = s[start..end].parse::<u64>().unwrap();

    let start = s.find("Each geode robot costs ").unwrap();
    let start = s[start..].find("ore and ").unwrap() + start + "ore and ".len();
    let end = s[start..].find(" ").unwrap();
    let geode_obsidian_cost = s[start..start + end].parse::<u64>().unwrap();

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

fn get_cost(s: &str, sep: &str) -> u64 {
    let start = s.find(sep).unwrap();
    let end = s[(start + sep.len())..].find(" ").unwrap();
    s[(start + sep.len()..(end + start + sep.len()))]
        .parse::<u64>()
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
        assert_eq!(fn1(s.as_str(), 22), 17);
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

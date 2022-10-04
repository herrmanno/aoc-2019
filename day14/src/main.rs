extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::{collections::{HashMap, VecDeque, HashSet}};

use anyhow::{Result};
use pest::{Parser, iterators::Pair};


/// A product (amount and type) that is a result of or input of an formula 
#[derive(Debug, Clone, PartialEq, Eq)]
struct Product(u64, String);

impl<'a> From<Pair<'a, Rule>> for Product {
    fn from(pair: Pair<'a, Rule>) -> Self {
        match pair.as_rule() {
            Rule::product => {
                match pair.clone().into_inner().collect::<Vec<Pair<'a, Rule>>>()[..] {
                    [ref count, ref name] =>
                        Product(count.as_str().parse().unwrap(), name.as_str().to_owned()),
                    _ => panic!("Cannot parse Product. Got unexpected tokens: {:?}", pair.into_inner())
                }
            }
            r => { panic!("Cannot convert {:?} to Product", r) }
        }
    }
}

/// A reaction/function from input products to an output product
#[derive(Debug)]
struct Formula(Vec<Product>, Product);

#[derive(Parser)]
#[grammar = "input.pest"]
struct InputParser;

impl<'a> From<Pair<'a, Rule>> for Formula {
    fn from(pair: Pair<'a, Rule>) -> Self {
        match pair.as_rule() {
            Rule::formula => {
                match pair.clone().into_inner().collect::<Vec<Pair<'a, Rule>>>()[..] {
                    [ref inputs, ref output] =>
                        Formula(
                            inputs.clone().into_inner().map(Product::from).collect(),
                            Product::from(output.clone())
                        ),
                    _ => panic!("Cannot parse Formula. Got unexpected tokens: {:?}", pair.into_inner())
                }
            }
            r => { panic!("Cannot convert {:?} to Formula", r) }
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let formulas = match args.get(1) {
        Some(path) => {
            match parse_input(path) {
                Ok(r) => { r }
                Err(e) => { panic!("{}", e); }
            }
        }
        None => panic!("Missing path argument")
    };

    solve2(&formulas);
}

fn solve2(formulas: &Vec<Formula>) {
    // create a map of all elements, that can *directly* be created from ORE only
    let ore_forward = formulas
        .iter()
        .filter_map(|formula| {
            match &formula.0[..] {
                [input] if input.1.eq("ORE") => Some((input.clone(), formula.1.clone())),
                _ => None
            }
        })
        .collect::<Vec<(Product, Product)>>();

    // create a mapping from
    // ```
    // element1 -> (amonut of element1, Product)
    // ```
    // where `amount of element1` units of element1 are needed to create the product (amount
    // and type) `product`
    let mut production_map = HashMap::new();
    let mut queue = ore_forward
        .iter()
        .map(|p| p.1.1.clone())
        .collect::<VecDeque<String>>();
    let mut seen = HashSet::<String>::new();
    while !queue.is_empty() {
        let item = queue.pop_front().unwrap();
        if seen.contains(&item) {
            continue;
        } else {
            seen.insert(item.clone());
        }
        for formula in formulas {
            formula.0
                .iter()
                .filter_map(|Product(amount, item2)| {
                    let production = &formula.1;
                    if item == *item2 {
                        if !seen.contains(&production.1) {
                            queue.push_back(production.1.clone())
                        }
                        Some((amount, production.clone()))
                    } else {
                        None
                    }
                })
                .for_each(|(&amount, production)| {
                    production_map
                        .entry(item.clone())
                        .and_modify(|v: &mut Vec<(u64, Product)>| { v.push((amount, production.clone())) })
                        .or_insert_with(|| vec![(amount, production)]);
                });
        }
    }

    // calculate amount or ORE needed to produce one unit of fuel
    let total: u64 = ore_forward
        .iter()
        .map(|(ore, item)| {
            let amount_item = amount_used(item.1.clone(), &production_map, 1);
            let count = (amount_item as f64 / item.0 as f64).ceil() as u64;
            ore.0 as u64 * count as u64
        })
        .sum();

    println!("{}", total);

    // calculate the max amount of fuel one can produce w/ one trillion units of ORE
    let mut min_fuel: u64 = 1;
    let mut max_fuel: u64 = 1_000_000_000_000;
    let mut fuel_count: u64 = 1;
    loop {
        let total: u64 = ore_forward
            .iter()
            .map(|(ore, item)| {
                let amount_item = amount_used(item.1.clone(), &production_map, fuel_count);
                let count = (amount_item as f64 / item.0 as f64).ceil() as u64;
                ore.0 as u64 * count as u64
            })
            .sum();

        if total < 1_000_000_000_000 {
            min_fuel = min_fuel.max(fuel_count);
            fuel_count = min_fuel + (max_fuel.abs_diff(min_fuel) / 2);
            if min_fuel + 1 == max_fuel {
                break;
            }
        } else if total > 1_000_000_000_000 {
            max_fuel = max_fuel.min(fuel_count);
            fuel_count = min_fuel + (max_fuel.abs_diff(min_fuel) / 2);
        } else if fuel_count + 1 == max_fuel {
            break;
        }
    }
    
    println!("{}", fuel_count);
}

/// Calcualte the used amount of a particular element to create `n` units of fuel
/// 
/// # Arguments
/// * `item`-  the element whose amount to calculate
/// * `productions` - the mapping from elements to the elements they can produce
/// * `fuel_count` - the amount of fuel to produce in the end
fn amount_used(
    item: String,
    productions: &HashMap<String, Vec<(u64, Product)>>,
    fuel_count: u64
) -> u64 {
    productions.get(&item).unwrap()
        .iter()
        .map(|(input_amount, output_product)| {
            if output_product.1 == "FUEL" {
                *input_amount * fuel_count
            } else {
                let parent_amount = amount_used(output_product.1.clone(), productions, fuel_count);
                input_amount * (parent_amount as f64 / output_product.0 as f64).ceil() as u64
            }
        })
        .sum()
}

/// Read file from path and parse to business data
fn parse_input(path: &str) -> Result<Vec<Formula>> {
    let s = std::fs::read_to_string(path)?;
    let mut r = InputParser::parse(Rule::formulas, &s)?;
    let formulas = r.next()
        .map(|pair| {
            pair.into_inner().map(Formula::from).collect::<Vec<Formula>>()
        })
        .unwrap();
    Ok(formulas)
}

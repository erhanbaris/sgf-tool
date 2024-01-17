use anyhow::{Result, bail};

use pest_derive::Parser;
use pest::{Parser, iterators::{Pair, Pairs}};

#[derive(Parser)]
#[grammar = "sgf.pest"]
struct IdentParser;


pub struct Variation {
    pub comment: Option<String>,
    pub variations: Vec<Box<Variation>>
}

pub enum Tree<'a> {
    Empty,
    Application(&'a str),
    Variation(Vec<Tree<'a>>)
}

fn parse_string(mut rules: Pairs<'_, Rule>) -> Result<&'_ str> {
    if let Some(inner_rule) = rules.next() {
        return Ok(inner_rule.as_str())
    }

    bail!("String is not valid")
}

fn parse_node(pair: Pair<'_, Rule>) -> Result<Tree<'_>> {
    let mut inner_rules = pair.into_inner();
    
    if let Some(rule) = inner_rules.next() {
        if let Rule::node_type = rule.as_rule() {
            rule.as_str();
            let result = match rule.as_str() {
                "ap" | "AP" => Tree::Application(parse_string(inner_rules)?),
                _ => Tree::Empty
            };
            return Ok(result);
        }    
    }

    bail!("Node is not valid")
}

fn parse_rule(pair: Pair<'_, Rule>) -> Result<Tree<'_>> {
    match pair.as_rule() {
        Rule::node => parse_node(pair),
        Rule::object => {
            println!(" > object");
            let mut items = Vec::new();
            
            for pair in pair.into_inner() {
                items.push(parse_rule(pair)?);
            }
            Ok(Tree::Variation(items))
        },
        Rule::EOI => { Ok(Tree::Empty) },
        unknown => {
            bail!("Unknown: {:?}", unknown)
        }
    }
}

fn parse_pair(pair: Pair<'_, Rule>) {
    for inner_pair in pair.into_inner() {
        parse_rule(inner_pair);
    }
}

pub fn parse(text: &str) {
    let pairs = IdentParser::parse(Rule::file, text).unwrap_or_else(|e| panic!("{}", e));
    
    for pair in pairs.into_iter() {
        parse_pair(pair);
    }
}
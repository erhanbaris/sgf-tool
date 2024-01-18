use anyhow::{Result, bail};

use thiserror::Error;

use pest_derive::Parser;
use pest::{Parser, iterators::{Pair, Pairs}};

#[derive(Parser)]
#[grammar = "sgf.pest"]
struct IdentParser;


#[derive(Error, Debug)]
pub enum SgfReaderError {
    #[error("Invalid number")]
    InvalidNumber,
}

#[derive(Debug)]
pub struct Stone<'a>(pub &'a str);

#[derive(Debug)]
pub enum Player {
    Black,
    White
}

#[derive(Debug)]
pub struct Base<'a> {
    pub items: Vec<Tree<'a>>
}

#[derive(Debug)]
pub enum Tree<'a> {
    Unknown(&'a str),
    Application(&'a str),
    Comment(&'a str),
    Copyright(&'a str),
    BlackName(&'a str),
    WhiteName(&'a str),
    BlackTeam(&'a str),
    WhiteTeam(&'a str),
    BoardSize(usize, usize),
    Variation(Vec<Tree<'a>>),
    FileFormat(usize),
    GameType(usize),
    Charset(&'a str),
    VariationShown(usize),
    WhoseTurn(Player),
    BlackStones(Vec<Stone<'a>>),
    WhiteStones(Vec<Stone<'a>>),
    BlackMove(Stone<'a>),
    WhiteMove(Stone<'a>),
    BlackPlayerRank(&'a str),
    WhitePlayerRank(&'a str),
    Source(&'a str),
    GameName(&'a str),
    NodeName(&'a str),
    Rule(&'a str),
    Komi(f32),
    PersonWhoProvidesAnnotations(&'a str),
    DrawArrow((Stone<'a>, Stone<'a>)),
    DrawCircle(Vec<Stone<'a>>),
    DrawSquare(Vec<Stone<'a>>),
    DrawTriangle(Vec<Stone<'a>>),
    GreyOut(Vec<Stone<'a>>),
    MarkX(Vec<Stone<'a>>),
    Handicap(usize),
    Result(&'a str),
}

fn parse_string(mut rules: Pairs<'_, Rule>) -> Result<&'_ str> {
    if let Some(inner_rule) = rules.next() {
        return Ok(inner_rule.as_str())
    }

    bail!("String is not valid")
}

fn parse_usize(mut rules: Pairs<'_, Rule>) -> Result<usize> {
    if let Some(inner_rule) = rules.next() {
        return Ok(inner_rule.as_str().parse::<usize>()?)
    }

    bail!("Number is not valid")
}

fn parse_float(mut rules: Pairs<'_, Rule>) -> Result<f32> {
    if let Some(inner_rule) = rules.next() {
        return Ok(inner_rule.as_str().parse::<f32>()?)
    }

    bail!("Number is not valid")
}

fn parse_player(mut rules: Pairs<'_, Rule>) -> Result<Player> {
    if let Some(inner_rule) = rules.next() {
        return Ok(match &inner_rule.as_str().to_uppercase()[..] {
            "B" => Player::Black,
            "W" => Player::White,
            _ => bail!("Player information not valid")
        })
    }

    bail!("Number is not valid")
}

fn parse_stones(rules: Pairs<'_, Rule>) -> Result<Vec<Stone<'_>>> {
    let mut stones = Vec::new();

    for rule in rules {
        stones.push(Stone(rule.as_str()));
    }

    Ok(stones)
}

fn parse_stone(mut rules: Pairs<'_, Rule>) -> Result<Stone<'_>> {
    if let Some(inner_rule) = rules.next() {
        return Ok(Stone(inner_rule.as_str()))
    }

    bail!("NumStoneber is not valid")
}

fn parse_two_stone(mut rules: Pairs<'_, Rule>) -> Result<(Stone<'_>, Stone<'_>)> {
    if let Some(inner_rule) = rules.next() {
        let stones = inner_rule.as_str().split(':').collect::<Vec<_>>();
        return Ok((Stone(stones[0]), Stone(stones[1])));
    }

    bail!("NumStoneber is not valid")
}

fn parse_node(pair: Pair<'_, Rule>) -> Result<Tree<'_>> {
    let mut inner_rules = pair.into_inner();
    
    if let Some(rule) = inner_rules.next() {
        if let Rule::node_type = rule.as_rule() {
            rule.as_str();
            let result = match &rule.as_str().to_uppercase()[..] {
                "AP" => Tree::Application(parse_string(inner_rules)?),
                "C" => Tree::Comment(parse_string(inner_rules)?),
                "CP" => Tree::Copyright(parse_string(inner_rules)?),
                "PB" => Tree::BlackName(parse_string(inner_rules)?),
                "PW" => Tree::WhiteName(parse_string(inner_rules)?),
                "BT" => Tree::BlackTeam(parse_string(inner_rules)?),
                "WT" => Tree::WhiteTeam(parse_string(inner_rules)?),
                "FF" => Tree::FileFormat(parse_usize(inner_rules)?),
                "GM" => Tree::GameType(parse_usize(inner_rules)?),
                "CA" => Tree::Charset(parse_string(inner_rules)?),
                "ST" => Tree::VariationShown(parse_usize(inner_rules)?),
                "PL" => Tree::WhoseTurn(parse_player(inner_rules)?),
                "AB" => Tree::BlackStones(parse_stones(inner_rules)?),
                "AW" => Tree::WhiteStones(parse_stones(inner_rules)?),
                "SO" => Tree::Source(parse_string(inner_rules)?),
                "GN" => Tree::GameName(parse_string(inner_rules)?),
                "N" => Tree::NodeName(parse_string(inner_rules)?),
                "B" => Tree::BlackMove(parse_stone(inner_rules)?),
                "W" => Tree::WhiteMove(parse_stone(inner_rules)?),
                "RU" => Tree::Rule(parse_string(inner_rules)?),
                "KM" => Tree::Komi(parse_float(inner_rules)?),
                "AR" => Tree::DrawArrow(parse_two_stone(inner_rules)?),
                "CR" => Tree::DrawCircle(parse_stones(inner_rules)?),
                "DD" => Tree::GreyOut(parse_stones(inner_rules)?),
                "MA" => Tree::MarkX(parse_stones(inner_rules)?),
                "SQ" => Tree::DrawSquare(parse_stones(inner_rules)?),
                "TR" => Tree::DrawTriangle(parse_stones(inner_rules)?),
                "AN" => Tree::PersonWhoProvidesAnnotations(parse_string(inner_rules)?),
                "BR" => Tree::BlackPlayerRank(parse_string(inner_rules)?),
                "WR" => Tree::WhitePlayerRank(parse_string(inner_rules)?),
                "HA" => Tree::Handicap(parse_usize(inner_rules)?),
                "RE" => Tree::Result(parse_string(inner_rules)?),
                "SZ" => {
                    let size = parse_usize(inner_rules)?;
                    Tree::BoardSize(size, size)
                },
                _ => Tree::Unknown(rule.as_str())
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
            let mut items = Vec::new();
            
            for pair in pair.into_inner() {
                items.push(parse_rule(pair)?);
            }
            Ok(Tree::Variation(items))
        },
        unknown => {
            bail!("Unknown: {:?}", unknown)
        }
    }
}

fn parse_pair(pair: Pair<'_, Rule>) -> Result<Base<'_>> {
    let mut base = Base {
        items: Vec::new()
    };
    
    for inner_pair in pair.into_inner() {
        base.items.push(parse_rule(inner_pair)?);
    }

    Ok(base)
}

pub fn parse(text: &str) -> Result<Base<'_>>{
    let pairs = IdentParser::parse(Rule::file, text).unwrap_or_else(|e| panic!("{}", e));

    if let Some(object) = pairs.into_iter().next() {
        return parse_pair(object);
    }

    bail!("Object not found");
}

// More detail https://homepages.cwi.nl/~aeb/go/misc/sgf.html#GM
// https://red-bean.com/sgf/properties.html#CR
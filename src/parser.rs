use crate::parser::inner_parser::{Rule, SgfParser};
use crate::{Base, Figure, Player, Point, PointRange, SgfToolError, StoneText, Tree};

use pest::iterators::{Pair, Pairs};
use pest::Parser;

pub(crate) mod inner_parser {
    use pest_derive::Parser;
    #[derive(Parser)]
    #[grammar = "sgf.pest"]
    pub(crate) struct SgfParser;
}

fn parse_string(mut rules: Pairs<'_, Rule>) -> Result<&'_ str, SgfToolError> {
    if let Some(inner_rule) = rules.next() {
        return Ok(inner_rule.as_str());
    }

    Err(SgfToolError::InvalidString)
}

fn parse_figure(mut rules: Pairs<'_, Rule>) -> Result<Option<Figure<'_>>, SgfToolError> {
    let node = match rules.next() {
        Some(node) => node.as_str(),
        None => return Ok(None),
    };

    if let Some(index) = node.find(':') {
        let text = &node[index + 1..];
        let number = node[0..index]
            .parse::<usize>()
            .map_err(|_| SgfToolError::InvalidNumber)?;
        return Ok(Some(Figure(number, text)));
    }

    Ok(None)
}

fn parse_usize(mut rules: Pairs<'_, Rule>) -> Result<usize, SgfToolError> {
    if let Some(inner_rule) = rules.next() {
        return inner_rule
            .as_str()
            .parse::<usize>()
            .map_err(|_| SgfToolError::InvalidNumber);
    }

    Err(SgfToolError::InvalidNumber)
}

fn parse_float(mut rules: Pairs<'_, Rule>) -> Result<f32, SgfToolError> {
    if let Some(inner_rule) = rules.next() {
        return inner_rule
            .as_str()
            .parse::<f32>()
            .map_err(|_| SgfToolError::InvalidFloat);
    }

    Err(SgfToolError::InvalidFloat)
}

fn parse_player(mut rules: Pairs<'_, Rule>) -> Result<Player, SgfToolError> {
    if let Some(inner_rule) = rules.next() {
        return Ok(match &inner_rule.as_str().to_uppercase()[..] {
            "B" => Player::Black,
            "W" => Player::White,
            _ => return Err(SgfToolError::PlayerInformationNotValid),
        });
    }

    Err(SgfToolError::InvalidNumber)
}

fn parse_stones(rules: Pairs<'_, Rule>) -> Result<Vec<Point<'_>>, SgfToolError> {
    let mut stones = Vec::new();

    for rule in rules {
        stones.push(Point(rule.as_str()));
    }

    Ok(stones)
}

fn parse_stone_texts(rules: Pairs<'_, Rule>) -> Result<Vec<StoneText<'_>>, SgfToolError> {
    let mut stone_texts = Vec::new();

    for rule in rules {
        if let Some(index) = rule.as_str().find(':') {
            let stone = Point(&rule.as_str()[index + 1..]);
            let text = &rule.as_str()[0..index];
            stone_texts.push(StoneText(stone, text));
        }
    }

    Ok(stone_texts)
}

fn parse_stone(mut rules: Pairs<'_, Rule>) -> Result<Point<'_>, SgfToolError> {
    if let Some(inner_rule) = rules.next() {
        return Ok(Point(inner_rule.as_str()));
    }

    Err(SgfToolError::InvalidNumber)
}

fn parse_point_range(mut rules: Pairs<'_, Rule>) -> Result<PointRange<'_>, SgfToolError> {
    if let Some(inner_rule) = rules.next() {
        let stones = inner_rule.as_str().split(':').collect::<Vec<_>>();
        return Ok(PointRange(Point(stones[0]), Point(stones[1])));
    }

    Err(SgfToolError::PointInformationNotValid)
}

fn parse_point_ranges(rules: Pairs<'_, Rule>) -> Result<Vec<PointRange<'_>>, SgfToolError> {
    let mut ranges = Vec::new();
    for rule in rules {
        let stones = rule.as_str().split(':').collect::<Vec<_>>();
        ranges.push(PointRange(Point(stones[0]), Point(stones[1])));
    }
    Ok(ranges)
}

fn parse_node(pair: Pair<'_, Rule>) -> Result<Tree<'_>, SgfToolError> {
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
                "AR" => Tree::DrawArrow(parse_point_range(inner_rules)?),
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
                "FG" => Tree::Figure(parse_figure(inner_rules)?),
                "PM" => Tree::Printing(parse_usize(inner_rules)?),
                "TM" => Tree::TimeLimit(parse_usize(inner_rules)?),
                "DT" => Tree::Date(parse_string(inner_rules)?),
                "EV" => Tree::Event(parse_string(inner_rules)?),
                "LB" => Tree::StoneText(parse_stone_texts(inner_rules)?),
                "RO" => Tree::Round(parse_string(inner_rules)?),
                "US" => Tree::SGFCreator(parse_string(inner_rules)?),
                "VW" => Tree::ViewOnly(parse_point_ranges(inner_rules)?),
                "MN" => Tree::MoveNumber(parse_usize(inner_rules)?),
                "SZ" => {
                    let size = parse_usize(inner_rules)?;
                    Tree::BoardSize(size, size)
                }
                _ => {
                    debug_assert!(false, "Unknown rule: {:?}", rule);
                    Tree::Unknown(rule.as_str())
                }
            };
            return Ok(result);
        }
    }

    Err(SgfToolError::NodeInformationNotValid)
}

fn parse_rule(pair: Pair<'_, Rule>) -> Result<Tree<'_>, SgfToolError> {
    match pair.as_rule() {
        Rule::node => parse_node(pair),
        Rule::object => {
            let mut items = Vec::new();

            for pair in pair.into_inner() {
                items.push(parse_rule(pair)?);
            }
            Ok(Tree::Variation(items))
        }
        _ => Err(SgfToolError::ParseFailed),
    }
}

fn parse_pair(pair: Pair<'_, Rule>) -> Result<Base<'_>, SgfToolError> {
    let mut base = Base { items: Vec::new() };

    for inner_pair in pair.into_inner() {
        base.items.push(parse_rule(inner_pair)?);
    }

    Ok(base)
}

pub fn parse(text: &str) -> Result<Base<'_>, SgfToolError> {
    let pairs = SgfParser::parse(Rule::file, text).map_err(|_| SgfToolError::SyntaxIssue)?;

    if let Some(object) = pairs.into_iter().next() {
        return parse_pair(object);
    }

    Err(SgfToolError::RootObjectNotFound)
}

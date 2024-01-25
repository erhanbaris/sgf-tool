use std::borrow::Cow;

use crate::parser::inner_parser::{Rule, SgfParser};
use crate::{Base, Figure, Move, Player, Point, PointRange, PointText, SgfToolError, Token};

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

fn parse_strings(mut rules: Pairs<'_, Rule>) -> Result<Vec<&'_ str>, SgfToolError> {
    let mut result = Vec::new();
    while let Some(inner_rule) = rules.next() {
        if Rule::node_value == inner_rule.as_rule() {
            result.push(inner_rule.as_str());
        }
    }

    Ok(result)
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

fn parse_stone_texts(rules: Pairs<'_, Rule>) -> Result<Vec<PointText<'_>>, SgfToolError> {
    let mut stone_texts = Vec::new();

    for rule in rules {
        if let Some(index) = rule.as_str().find(':') {
            let stone = Point(&rule.as_str()[index + 1..]);
            let text = &rule.as_str()[0..index];
            stone_texts.push(PointText(stone, text));
        }
    }

    Ok(stone_texts)
}

fn parse_move(mut rules: Pairs<'_, Rule>) -> Result<Move<'_>, SgfToolError> {
    if let Some(inner_rule) = rules.next() {
        return Ok(Move::Move(Point(inner_rule.as_str())));
    }
    Ok(Move::Pass)
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

fn parse_node(pair: Pair<'_, Rule>) -> Result<Token<'_>, SgfToolError> {
    let mut inner_rules = pair.into_inner();

    if let Some(rule) = inner_rules.next() {
        if let Rule::node_type = rule.as_rule() {
            rule.as_str();
            let result = match &rule.as_str().to_uppercase()[..] {
                "AP" => Token::Application(parse_string(inner_rules)?),
                "C" => Token::Comment(parse_string(inner_rules)?),
                "CP" => Token::Copyright(parse_string(inner_rules)?),
                "PB" => Token::BlackName(parse_string(inner_rules)?),
                "PW" => Token::WhiteName(parse_string(inner_rules)?),
                "BT" => Token::BlackTeam(parse_string(inner_rules)?),
                "WT" => Token::WhiteTeam(parse_string(inner_rules)?),
                "FF" => Token::FileFormat(parse_usize(inner_rules)?),
                "GM" => Token::GameType(parse_usize(inner_rules)?),
                "CA" => Token::Charset(parse_string(inner_rules)?),
                "ST" => Token::VariationShown(parse_usize(inner_rules)?),
                "PL" => Token::WhoseTurn(parse_player(inner_rules)?),
                "AB" => Token::BlackStones(parse_stones(inner_rules)?),
                "AW" => Token::WhiteStones(parse_stones(inner_rules)?),
                "SO" => Token::Source(parse_string(inner_rules)?),
                "GN" => Token::GameName(parse_string(inner_rules)?),
                "N" => Token::NodeName(parse_string(inner_rules)?),
                "B" => Token::BlackMove(parse_move(inner_rules)?),
                "W" => Token::WhiteMove(parse_move(inner_rules)?),
                "RU" => Token::Rule(parse_string(inner_rules)?),
                "KM" => Token::Komi(parse_float(inner_rules)?),
                "AR" => Token::DrawArrow(parse_point_range(inner_rules)?),
                "CR" => Token::DrawCircle(parse_stones(inner_rules)?),
                "DD" => Token::GreyOut(parse_stones(inner_rules)?),
                "MA" => Token::MarkX(parse_stones(inner_rules)?),
                "SQ" => Token::DrawSquare(parse_stones(inner_rules)?),
                "TR" => Token::DrawTriangle(parse_stones(inner_rules)?),
                "AN" => Token::PersonWhoProvidesAnnotations(parse_string(inner_rules)?),
                "BR" => Token::BlackPlayerRank(parse_string(inner_rules)?),
                "WR" => Token::WhitePlayerRank(parse_string(inner_rules)?),
                "HA" => Token::Handicap(parse_usize(inner_rules)?),
                "RE" => Token::Result(parse_string(inner_rules)?),
                "FG" => Token::Figure(parse_figure(inner_rules)?),
                "PM" => Token::Printing(parse_usize(inner_rules)?),
                "TM" => Token::TimeLimit(parse_usize(inner_rules)?),
                "DT" => Token::Date(parse_string(inner_rules)?),
                "EV" => Token::Event(parse_string(inner_rules)?),
                "LB" => Token::PointText(parse_stone_texts(inner_rules)?),
                "RO" => Token::Round(parse_string(inner_rules)?),
                "US" => Token::SGFCreator(parse_string(inner_rules)?),
                "VW" => Token::ViewOnly(parse_point_ranges(inner_rules)?),
                "MN" => Token::MoveNumber(parse_usize(inner_rules)?),
                "AE" => Token::ClearPoints(parse_stones(inner_rules)?),
                "SZ" => {
                    let size = parse_usize(inner_rules)?;
                    Token::BoardSize(size, size)
                }
                _ => Token::Unknown(rule.as_str(), parse_strings(inner_rules)?)
            };
            return Ok(result);
        }
    }

    Err(SgfToolError::NodeInformationNotValid)
}

fn parse_rule(pair: Pair<'_, Rule>) -> Result<Token<'_>, SgfToolError> {
    match pair.as_rule() {
        Rule::node => parse_node(pair),
        Rule::object => {
            let mut base = Base::default();

            for pair in pair.into_inner() {
                base.tokens.push(Cow::Owned(parse_rule(pair)?));
            }
            Ok(Token::Variation(base))
        }
        _ => Err(SgfToolError::ParseFailed),
    }
}

fn parse_pair(pair: Pair<'_, Rule>) -> Result<Base<'_>, SgfToolError> {
    let mut base = Base { tokens: Vec::new() };

    for inner_pair in pair.into_inner() {
        base.tokens.push(Cow::Owned(parse_rule(inner_pair)?));
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

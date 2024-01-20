use crate::{Base, Figure, Player, Point, PointRange, SgfToolError, StoneText, Token};
use std::string::String;

use strum::EnumMessage;

pub trait Builder {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError>;
}

impl Builder for Point<'_> {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        buffer.push_str(self.0);
        Ok(())
    }
}

impl Builder for PointRange<'_> {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        self.0.build(buffer)?;
        buffer.push(':');
        self.1.build(buffer)?;
        Ok(())
    }
}

impl Builder for Figure<'_> {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        buffer.push_str(&self.0.to_string());
        buffer.push(':');
        buffer.push_str(self.1);
        Ok(())
    }
}

impl Builder for StoneText<'_> {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        self.0.build(buffer)?;
        buffer.push(':');
        buffer.push_str(self.1);
        Ok(())
    }
}

impl Builder for Player {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        buffer.push(match self {
            Player::Black => 'B',
            Player::White => 'W',
        });
        Ok(())
    }
}

impl Builder for Base<'_> {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        buffer.push('(');
        for node in self.tokens.iter() {
            node.build(buffer)?;
        }
        buffer.push(')');
        Ok(())
    }
}

impl Builder for &'_ str {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        buffer.push_str(self);
        Ok(())
    }
}

impl<T: Builder> Builder for Vec<T> {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        for point in self.iter() {
            buffer.push('[');
            point.build(buffer)?;
            buffer.push(']');
        }
        Ok(())
    }
}

impl Builder for Option<Figure<'_>> {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        if let Some(item) = self {
            item.build(buffer)?;
        }

        Ok(())
    }
}

impl Builder for usize {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        buffer.push_str(&self.to_string());
        Ok(())
    }
}

impl Builder for f32 {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        buffer.push_str(&self.to_string());
        Ok(())
    }
}

impl Builder for Token<'_> {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        let node_type = self.get_message();

        match self {
            Token::Unknown(item) => add_node(buffer, node_type, item),
            Token::Application(application) => add_node(buffer, node_type, application),
            Token::Comment(comment) => add_node(buffer, node_type, comment),
            Token::Copyright(copyright) => add_node(buffer, node_type, copyright),
            Token::BlackName(name) => add_node(buffer, node_type, name),
            Token::WhiteName(name) => add_node(buffer, node_type, name),
            Token::BlackTeam(name) => add_node(buffer, node_type, name),
            Token::WhiteTeam(name) => add_node(buffer, node_type, name),
            Token::BoardSize(size, _) => add_node(buffer, node_type, size),
            Token::Variation(variants) => {
                buffer.push('(');
                for variant in variants.iter() {
                    variant.build(buffer)?;
                }
                buffer.push(')');
                Ok(())
            }
            Token::FileFormat(format) => add_node(buffer, node_type, format),
            Token::GameType(game) => add_node(buffer, node_type, game),
            Token::Charset(charset) => add_node(buffer, node_type, charset),
            Token::VariationShown(show) => add_node(buffer, node_type, show),
            Token::WhoseTurn(turn) => add_node(buffer, node_type, turn),
            Token::BlackStones(stones) => add_multiple_nodes(buffer, node_type, stones),
            Token::WhiteStones(stones) => add_multiple_nodes(buffer, node_type, stones),
            Token::BlackMove(point) => add_node(buffer, node_type, point),
            Token::WhiteMove(point) => add_node(buffer, node_type, point),
            Token::BlackPlayerRank(rank) => add_node(buffer, node_type, rank),
            Token::WhitePlayerRank(rank) => add_node(buffer, node_type, rank),
            Token::Source(source) => add_node(buffer, node_type, source),
            Token::GameName(name) => add_node(buffer, node_type, name),
            Token::NodeName(name) => add_node(buffer, node_type, name),
            Token::Rule(rule) => add_node(buffer, node_type, rule),
            Token::Komi(komi) => add_node(buffer, node_type, komi),
            Token::PersonWhoProvidesAnnotations(person) => add_node(buffer, node_type, person),
            Token::DrawArrow(point_range) => add_node(buffer, node_type, point_range),
            Token::DrawCircle(points) => add_multiple_nodes(buffer, node_type, points),
            Token::DrawSquare(item) => add_multiple_nodes(buffer, node_type, item),
            Token::DrawTriangle(item) => add_multiple_nodes(buffer, node_type, item),
            Token::GreyOut(item) => add_multiple_nodes(buffer, node_type, item),
            Token::MarkX(item) => add_multiple_nodes(buffer, node_type, item),
            Token::Handicap(item) => add_node(buffer, node_type, item),
            Token::Result(item) => add_node(buffer, node_type, item),
            Token::Figure(item) => add_node(buffer, node_type, item),
            Token::Printing(item) => add_node(buffer, node_type, item),
            Token::TimeLimit(item) => add_node(buffer, node_type, item),
            Token::Date(item) => add_node(buffer, node_type, item),
            Token::Event(item) => add_node(buffer, node_type, item),
            Token::StoneText(item) => add_multiple_nodes(buffer, node_type, item),
            Token::Round(item) => add_node(buffer, node_type, item),
            Token::SGFCreator(item) => add_node(buffer, node_type, item),
            Token::ViewOnly(item) => add_multiple_nodes(buffer, node_type, item),
            Token::MoveNumber(item) => add_node(buffer, node_type, item),
        }
    }
}

fn add_node<T: Builder + ?Sized>(
    buffer: &mut String,
    node_type: Option<&'static str>,
    node_value: &T,
) -> Result<(), SgfToolError> {
    if let Some(message) = node_type {
        buffer.push(';');
        buffer.push_str(message);
        buffer.push('[');
        node_value.build(buffer)?;
        buffer.push(']');
    }
    Ok(())
}

fn add_multiple_nodes<T: Builder + ?Sized>(
    buffer: &mut String,
    node_type: Option<&'static str>,
    node_value: &T,
) -> Result<(), SgfToolError> {
    if let Some(message) = node_type {
        buffer.push(';');
        buffer.push_str(message);
        node_value.build(buffer)?;
    }
    Ok(())
}

pub fn build(object: Base<'_>) -> Result<String, SgfToolError> {
    let mut buffer = String::new();
    object.build(&mut buffer)?;
    Ok(buffer)
}

use crate::{Base, Figure, Player, Point, PointRange, SgfToolError, StoneText, Tree};
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
        for node in self.items.iter() {
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

impl Builder for Tree<'_> {
    fn build(&self, buffer: &mut String) -> Result<(), SgfToolError> {
        let node_type = self.get_message();

        match self {
            Tree::Unknown(item) => add_node(buffer, node_type, item),
            Tree::Application(application) => add_node(buffer, node_type, application),
            Tree::Comment(comment) => add_node(buffer, node_type, comment),
            Tree::Copyright(copyright) => add_node(buffer, node_type, copyright),
            Tree::BlackName(name) => add_node(buffer, node_type, name),
            Tree::WhiteName(name) => add_node(buffer, node_type, name),
            Tree::BlackTeam(name) => add_node(buffer, node_type, name),
            Tree::WhiteTeam(name) => add_node(buffer, node_type, name),
            Tree::BoardSize(size, _) => add_node(buffer, node_type, size),
            Tree::Variation(variants) => {
                buffer.push('(');
                for variant in variants.iter() {
                    variant.build(buffer)?;
                }
                buffer.push(')');
                Ok(())
            }
            Tree::FileFormat(format) => add_node(buffer, node_type, format),
            Tree::GameType(game) => add_node(buffer, node_type, game),
            Tree::Charset(charset) => add_node(buffer, node_type, charset),
            Tree::VariationShown(show) => add_node(buffer, node_type, show),
            Tree::WhoseTurn(turn) => add_node(buffer, node_type, turn),
            Tree::BlackStones(stones) => add_multiple_nodes(buffer, node_type, stones),
            Tree::WhiteStones(stones) => add_multiple_nodes(buffer, node_type, stones),
            Tree::BlackMove(point) => add_node(buffer, node_type, point),
            Tree::WhiteMove(point) => add_node(buffer, node_type, point),
            Tree::BlackPlayerRank(rank) => add_node(buffer, node_type, rank),
            Tree::WhitePlayerRank(rank) => add_node(buffer, node_type, rank),
            Tree::Source(source) => add_node(buffer, node_type, source),
            Tree::GameName(name) => add_node(buffer, node_type, name),
            Tree::NodeName(name) => add_node(buffer, node_type, name),
            Tree::Rule(rule) => add_node(buffer, node_type, rule),
            Tree::Komi(komi) => add_node(buffer, node_type, komi),
            Tree::PersonWhoProvidesAnnotations(person) => add_node(buffer, node_type, person),
            Tree::DrawArrow(point_range) => add_node(buffer, node_type, point_range),
            Tree::DrawCircle(points) => add_multiple_nodes(buffer, node_type, points),
            Tree::DrawSquare(item) => add_multiple_nodes(buffer, node_type, item),
            Tree::DrawTriangle(item) => add_multiple_nodes(buffer, node_type, item),
            Tree::GreyOut(item) => add_multiple_nodes(buffer, node_type, item),
            Tree::MarkX(item) => add_multiple_nodes(buffer, node_type, item),
            Tree::Handicap(item) => add_node(buffer, node_type, item),
            Tree::Result(item) => add_node(buffer, node_type, item),
            Tree::Figure(item) => add_node(buffer, node_type, item),
            Tree::Printing(item) => add_node(buffer, node_type, item),
            Tree::TimeLimit(item) => add_node(buffer, node_type, item),
            Tree::Date(item) => add_node(buffer, node_type, item),
            Tree::Event(item) => add_node(buffer, node_type, item),
            Tree::StoneText(item) => add_multiple_nodes(buffer, node_type, item),
            Tree::Round(item) => add_node(buffer, node_type, item),
            Tree::SGFCreator(item) => add_node(buffer, node_type, item),
            Tree::ViewOnly(item) => add_multiple_nodes(buffer, node_type, item),
            Tree::MoveNumber(item) => add_node(buffer, node_type, item),
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

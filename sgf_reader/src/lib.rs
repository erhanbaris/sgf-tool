use parser::Rule;
use pest::Parser;
use serde::{Deserialize, Serialize};

use thiserror::Error;
use crate::parser::SgfParser;
use pest::iterators::{Pair, Pairs};

pub(crate) mod parser {
    use pest_derive::Parser;
    #[derive(Parser)]
    #[grammar = "sgf.pest"]
    pub(crate) struct SgfParser;
}

#[derive(Error, Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum SgfReaderError {
    #[error("Syntax issue")]
    SyntaxIssue,

    #[error("Root object not found")]
    RootObjectNotFound,

    #[error("Parse failed")]
    ParseFailed,

    #[error("Invalid number")]
    InvalidNumber,

    #[error("Invalid string")]
    InvalidString,

    #[error("Invalid float")]
    InvalidFloat,

    #[error("Player information not valid")]
    PlayerInformationNotValid,

    #[error("Point information not valid")]
    PointInformationNotValid,

    #[error("Node information not valid")]
    NodeInformationNotValid,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Point<'a>(pub &'a str);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct PointRange<'a>(#[serde(borrow)] pub Point<'a>, pub Point<'a>);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Figure<'a>(pub usize, pub &'a str);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct StoneText<'a>(pub Point<'a>, pub &'a str);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum Player {
    Black,
    White
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Base<'a> {
    #[serde(borrow)]
    pub items: Vec<Tree<'a>>
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum Tree<'a> {
    Unknown(&'a str),
    
    /// Property: AP
    Application(&'a str),
    
    /// Property: C
    Comment(&'a str),
    
    /// Property: CP
    Copyright(&'a str),
    
    /// Property: PB
    BlackName(&'a str),
    
    /// Property: PW
    WhiteName(&'a str),
    
    /// Property: BT
    BlackTeam(&'a str),
    
    /// Property: WT
    WhiteTeam(&'a str),
    
    /// Property: SZ
    BoardSize(usize, usize),
    
    /// Variation
    Variation(Vec<Tree<'a>>),
    
    /// Property: FF 
    FileFormat(usize),
    
    /// Property: GM
    GameType(usize),
    
    /// Property: CA
    Charset(&'a str),
    
    /// Property: ST
    VariationShown(usize),
    
    /// Property: PL
    WhoseTurn(Player),
    
    /// Property: AB
    BlackStones(Vec<Point<'a>>),
    
    /// Property: AW
    WhiteStones(Vec<Point<'a>>),
    
    /// Property: B
    BlackMove(Point<'a>),
    
    /// Property: W
    WhiteMove(Point<'a>),
    
    /// Property: BR
    BlackPlayerRank(&'a str),
    
    /// Property: WR
    WhitePlayerRank(&'a str),
    
    /// Property: SO
    Source(&'a str),
    
    /// Property: GN
    GameName(&'a str),
    
    /// Property: N
    NodeName(&'a str),
    
    /// Property: RU
    Rule(&'a str),
    
    /// Property: KM
    Komi(f32),
    
    /// Property: AN
    PersonWhoProvidesAnnotations(&'a str),
    
    /// Property: AR
    DrawArrow(PointRange<'a>),
    
    /// Property: CR
    DrawCircle(Vec<Point<'a>>),
    
    /// Property: SQ
    DrawSquare(Vec<Point<'a>>),
    
    /// Property: TR
    DrawTriangle(Vec<Point<'a>>),
    
    /// Property: DD
    GreyOut(Vec<Point<'a>>),
    
    /// Property: MA
    MarkX(Vec<Point<'a>>),
    
    /// Property: HA
    Handicap(usize),
    
    /// Property: RE
    Result(&'a str),
    
    /// Property: FG
    Figure(Option<Figure<'a>>),
    
    /// Property: PM
    Printing(usize),
    
    /// Property: TM
    TimeLimit(usize),
    
    /// Property: DT
    Date(&'a str),
    
    /// Property: AV
    Event(&'a str),
    
    /// Property: LB
    StoneText(Vec<StoneText<'a>>),
    
    /// Property: RO
    Round(&'a str),
    
    /// Property: US
    SGFCreator(&'a str),

    /// Property: VW
    ViewOnly(Vec<PointRange<'a>>),
    
    /// Property: MN
    MoveNumber(usize)
}

fn parse_string(mut rules: Pairs<'_, Rule>) -> Result<&'_ str, SgfReaderError> {
    if let Some(inner_rule) = rules.next() {
        return Ok(inner_rule.as_str())
    }

    Err(SgfReaderError::InvalidString)
}

fn parse_figure(mut rules: Pairs<'_, Rule>) -> Result<Option<Figure<'_>>, SgfReaderError> {

    let node = match rules.next() {
        Some(node) => node.as_str(),
        None => return Ok(None)
    };

    if let Some(index) = node.find(':') {
        let text = &node[index+1..];
        let number = node[0..index].parse::<usize>().map_err(|_| SgfReaderError::InvalidNumber)?;
        return Ok(Some(Figure(number, text)))
    }

    Ok(None)
}

fn parse_usize(mut rules: Pairs<'_, Rule>) -> Result<usize, SgfReaderError> {
    if let Some(inner_rule) = rules.next() {
        return inner_rule.as_str().parse::<usize>().map_err(|_| SgfReaderError::InvalidNumber)
    }

    Err(SgfReaderError::InvalidNumber)
}

fn parse_float(mut rules: Pairs<'_, Rule>) -> Result<f32, SgfReaderError> {
    if let Some(inner_rule) = rules.next() {
        return inner_rule.as_str().parse::<f32>().map_err(|_| SgfReaderError::InvalidFloat)
    }

    Err(SgfReaderError::InvalidFloat)
}

fn parse_player(mut rules: Pairs<'_, Rule>) -> Result<Player, SgfReaderError> {
    if let Some(inner_rule) = rules.next() {
        return Ok(match &inner_rule.as_str().to_uppercase()[..] {
            "B" => Player::Black,
            "W" => Player::White,
            _ => return Err(SgfReaderError::PlayerInformationNotValid)
        })
    }

    Err(SgfReaderError::InvalidNumber)
}

fn parse_stones(rules: Pairs<'_, Rule>) -> Result<Vec<Point<'_>>, SgfReaderError> {
    let mut stones = Vec::new();

    for rule in rules {
        stones.push(Point(rule.as_str()));
    }

    Ok(stones)
}

fn parse_stone_texts(rules: Pairs<'_, Rule>) -> Result<Vec<StoneText<'_>>, SgfReaderError> {
    let mut stone_texts = Vec::new();

    for rule in rules {
        if let Some(index) = rule.as_str().find(':') {
            let stone = Point(&rule.as_str()[index+1..]);
            let text = &rule.as_str()[0..index];
            stone_texts.push(StoneText(stone, text));
        }
    }

    Ok(stone_texts)
}

fn parse_stone(mut rules: Pairs<'_, Rule>) -> Result<Point<'_>, SgfReaderError> {
    if let Some(inner_rule) = rules.next() {
        return Ok(Point(inner_rule.as_str()))
    }

    Err(SgfReaderError::InvalidNumber)
}

fn parse_point_range(mut rules: Pairs<'_, Rule>) -> Result<PointRange<'_>, SgfReaderError> {
    if let Some(inner_rule) = rules.next() {
        let stones = inner_rule.as_str().split(':').collect::<Vec<_>>();
        return Ok(PointRange(Point(stones[0]), Point(stones[1])));
    }

    Err(SgfReaderError::PointInformationNotValid)
}

fn parse_point_ranges(rules: Pairs<'_, Rule>) -> Result<Vec<PointRange<'_>>, SgfReaderError> {
    let mut ranges = Vec::new();
    for rule in rules {
        let stones = rule.as_str().split(':').collect::<Vec<_>>();
        ranges.push(PointRange(Point(stones[0]), Point(stones[1])));
    }
    Ok(ranges)
}

fn parse_node(pair: Pair<'_, Rule>) -> Result<Tree<'_>, SgfReaderError> {
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
                },
                _ => {
                    debug_assert!(false, "Unknown rule: {:?}", rule);
                    Tree::Unknown(rule.as_str())
                }
            };
            return Ok(result);
        }    
    }

    Err(SgfReaderError::NodeInformationNotValid)
}

fn parse_rule(pair: Pair<'_, Rule>) -> Result<Tree<'_>, SgfReaderError> {
    match pair.as_rule() {
        Rule::node => parse_node(pair),
        Rule::object => {
            let mut items = Vec::new();
            
            for pair in pair.into_inner() {
                items.push(parse_rule(pair)?);
            }
            Ok(Tree::Variation(items))
        },
        _ => {
            Err(SgfReaderError::ParseFailed)
        }
    }
}

fn parse_pair(pair: Pair<'_, Rule>) -> Result<Base<'_>, SgfReaderError> {
    let mut base = Base {
        items: Vec::new()
    };
    
    for inner_pair in pair.into_inner() {
        base.items.push(parse_rule(inner_pair)?);
    }

    Ok(base)
}

pub fn parse(text: &str) -> Result<Base<'_>, SgfReaderError>{
    let pairs = SgfParser::parse(Rule::file, text).map_err(|_| SgfReaderError::SyntaxIssue)?;

    if let Some(object) = pairs.into_iter().next() {
        return parse_pair(object);
    }

    Err(SgfReaderError::RootObjectNotFound)
}

// More detail https://homepages.cwi.nl/~aeb/go/misc/sgf.html#GM
// https://red-bean.com/sgf/properties.html#CR


#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn basic_sgf_parse() -> Result<(), SgfReaderError>{
        let result = parse("()")?;
        assert_eq!(result.items.len(), 0);
        assert_eq!(parse("(a)"), Err(SgfReaderError::SyntaxIssue));
        assert_eq!(parse("(1)"), Err(SgfReaderError::SyntaxIssue));
        assert_eq!(parse("("), Err(SgfReaderError::SyntaxIssue));
        assert_eq!(parse(")"), Err(SgfReaderError::SyntaxIssue));
        assert_eq!(parse(""), Err(SgfReaderError::SyntaxIssue));
        assert_eq!(parse("-"), Err(SgfReaderError::SyntaxIssue));
        assert_eq!(parse(" "), Err(SgfReaderError::SyntaxIssue));
        Ok(())
    }

    #[test]
    fn sgf_parse() -> Result<(), SgfReaderError>{
        let result = parse(r#"(
    ;FF[4]
    C[root]
    (
        ;C[a]
        ;C[b]
        (
            ;C[c]
        )
        (
            ;C[d]
            ;C[e]
        )
    )
    (
        ;C[f]
        (
            ;C[g]
            ;C[h]
            ;C[i]
        )
        (
            ;C[j]
        )
    )
)"#
)?;
        assert_eq!(result.items.len(), 4);
        assert_eq!(result.items[0], Tree::FileFormat(4));
        assert_eq!(result.items[1], Tree::Comment("root"));

        if let Tree::Variation(trees) = &result.items[2] {
            assert_eq!(trees.len(), 4);
            assert_eq!(trees[0], Tree::Comment("a"));
            assert_eq!(trees[1], Tree::Comment("b"));

            if let Tree::Variation(trees) = &trees[2] {
                assert_eq!(trees.len(), 1);
                assert_eq!(trees[0], Tree::Comment("c"));
            }
            else {
                assert!(false, "Variation not found");
            }

            if let Tree::Variation(trees) = &trees[3] {
                assert_eq!(trees.len(), 2);
                assert_eq!(trees[0], Tree::Comment("d"));
                assert_eq!(trees[1], Tree::Comment("e"));
            }
            else {
                assert!(false, "Variation not found");
            }
        }
        else {
            assert!(false, "Variation not found");
        }

        if let Tree::Variation(trees) = &result.items[3] {
            assert_eq!(trees.len(), 3);
            assert_eq!(trees[0], Tree::Comment("f"));

            if let Tree::Variation(trees) = &trees[1] {
                assert_eq!(trees.len(), 3);
                assert_eq!(trees[0], Tree::Comment("g"));
                assert_eq!(trees[1], Tree::Comment("h"));
                assert_eq!(trees[2], Tree::Comment("i"));
            }
            else {
                assert!(false, "Variation not found");
            }

            if let Tree::Variation(trees) = &trees[2] {
                assert_eq!(trees.len(), 1);
                assert_eq!(trees[0], Tree::Comment("j"));
            }
            else {
                assert!(false, "Variation not found");
            }
        }
        else {
            assert!(false, "Variation not found");
        }

        parse(r#"
        (;FF[4]GM[1]SZ[19]FG[257:Figure 1]PM[1]
            PB[Takemiya Masaki]BR[9 dan]PW[Cho Chikun]
            WR[9 dan]RE[W+Resign]KM[5.5]TM[28800]DT[1996-10-18,19]
            EV[21st Meijin]RO[2 (final)]SO[Go World #78]US[Arno Hollosi]
            ;B[pd];W[dp];B[pp];W[dd];B[pj];W[nc];B[oe];W[qc];B[pc];W[qd]
            (;B[qf];W[rf];B[rg];W[re];B[qg];W[pb];B[ob];W[qb]
            (;B[mp];W[fq];B[ci];W[cg];B[dl];W[cn];B[qo];W[ec];B[jp];W[jd]
            ;B[ei];W[eg];B[kk]LB[qq:a][dj:b][ck:c][qp:d]N[Figure 1]
            
            ;W[me]FG[257:Figure 2];B[kf];W[ke];B[lf];W[jf];B[jg]
            (;W[mf];B[if];W[je];B[ig];W[mg];B[mj];W[mq];B[lq];W[nq]
            (;B[lr];W[qq];B[pq];W[pr];B[rq];W[rr];B[rp];W[oq];B[mr];W[oo];B[mn]
            (;W[nr];B[qp]LB[kd:a][kh:b]N[Figure 2]
            
            ;W[pk]FG[257:Figure 3];B[pm];W[oj];B[ok];W[qr];B[os];W[ol];B[nk];W[qj]
            ;B[pi];W[pl];B[qm];W[ns];B[sr];W[om];B[op];W[qi];B[oi]
            (;W[rl];B[qh];W[rm];B[rn];W[ri];B[ql];W[qk];B[sm];W[sk];B[sh];W[og]
            ;B[oh];W[np];B[no];W[mm];B[nn];W[lp];B[kp];W[lo];B[ln];W[ko];B[mo]
            ;W[jo];B[km]N[Figure 3])
            
            (;W[ql]VW[ja:ss]FG[257:Dia. 6]MN[1];B[rm];W[ph];B[oh];W[pg];B[og];W[pf]
            ;B[qh];W[qe];B[sh];W[of];B[sj]TR[oe][pd][pc][ob]LB[pe:a][sg:b][si:c]
            N[Diagram 6]))
            
            (;W[no]VW[jj:ss]FG[257:Dia. 5]MN[1];B[pn]N[Diagram 5]))
            
            (;B[pr]FG[257:Dia. 4]MN[1];W[kq];B[lp];W[lr];B[jq];W[jr];B[kp];W[kr];B[ir]
            ;W[hr]LB[is:a][js:b][or:c]N[Diagram 4]))
            
            (;W[if]FG[257:Dia. 3]MN[1];B[mf];W[ig];B[jh]LB[ki:a]N[Diagram 3]))
            
            (;W[oc]VW[aa:sk]FG[257:Dia. 2]MN[1];B[md];W[mc];B[ld]N[Diagram 2]))
            
            (;B[qe]VW[aa:sj]FG[257:Dia. 1]MN[1];W[re];B[qf];W[rf];B[qg];W[pb];B[ob]
            ;W[qb]LB[rg:a]N[Diagram 1]))
             "#)?;

             parse(r#"(;FF[4]GM[1]SZ[19]FG[257:Figure 1]PM[2]
                PB[Cho Chikun]BR[9 dan]PW[Ryu Shikun]WR[9 dan]RE[W+2.5]KM[5.5]
                DT[1996-08]EV[51st Honinbo]RO[5 (final)]SO[Go World #78]US[Arno Hollosi]
                ;B[qd];W[dd];B[fc];W[df];B[pp];W[dq];B[kc];W[cn];B[pj];W[jp];B[lq];W[oe]
                ;B[pf];W[ke];B[id];W[lc];B[lb];W[kb];B[jb];W[kd];B[ka];W[jc];B[ic];W[kb]
                ;B[mc];W[qc]N[Figure 1]
                
                ;B[pd]FG[257:Figure 2];W[pc];B[od];W[oc];B[kc];W[nd];B[nc];W[kb];B[rd];W[pe]
                (;B[rf];W[md];B[kc];W[qe];B[re];W[kb];B[mb];W[qf];B[qg];W[pg];B[qh];W[kc]
                ;B[hb];W[nf];B[ch];W[cj];B[eh];W[ob]
                (;B[cc];W[dc];B[db];W[bf];B[bb]
                ;W[bh]LB[of:a][mf:b][rc:c][di:d][ja:e]N[Figure 2]
                
                ;B[qp]FG[257:Figure 3];W[lo];B[ej];W[oq]
                (;B[np];W[mq];B[mp];W[lp]
                (;B[kq];W[nq];B[op];W[jq];B[mr];W[nr];B[lr];W[qr];B[jr];W[ir];B[hr];W[iq]
                ;B[is];W[ks];B[js];W[gq];B[gr];W[fq];B[pq];W[pr];B[ns];W[or];B[rq];W[hq]
                ;B[rr];W[cl];B[cg];W[bg];B[og];W[ng]
                (;B[ci];W[bi];B[dj];W[dk];B[mm];W[gk];B[gi];W[mn];B[nm];W[kl];B[nh];W[mh]
                ;B[mi];W[li];B[lh];W[mg];B[ek];W[el];B[ik]LB[kr:a]N[Figure 3]
                
                ;W[ki]FG[257:Figure 4];B[fl];W[fk];B[gl];W[hk];B[hl];W[hj];B[jl];W[kk];B[km]
                ;W[lm];B[ll];W[jm];B[jj];W[ji];B[kj];W[lj];B[ij];W[hi];B[em];W[dl];B[ii]
                ;W[hh];B[ih];W[hg];B[ln];W[kn];B[lm];W[im];B[il];W[fg];B[lk];W[ni];B[ef]
                ;W[eg];B[dg];W[ff];B[oh];W[of];B[oj];W[ph];B[oi];W[mj];B[ee];W[fe];B[de]
                ;W[ed];B[ce];W[cf];B[rb];W[rc];B[sc];W[qb];B[sb];W[la];B[ma];W[na];B[ja]
                ;W[nb];B[la];W[pa];B[be];W[fd];B[bj];W[ck];B[ec];W[hs];B[gs];W[fr];B[os]
                ;W[ps];B[ms];W[nk];B[ok];W[kp];B[fo];W[fs];B[qq];W[hs];B[do];W[co];B[ig]
                ;W[gc];B[gb];W[jf];B[di];W[fi];B[hf];W[gf];B[af];W[mo];B[he];W[kr];B[qs]
                ;W[no];B[oo];W[nn];B[on];W[nl];B[ol];W[gn];B[fn];W[in];B[nj];W[mk];B[jg]
                ;W[kg];B[mi];W[jh];B[ag];W[bk];B[ah];W[aj];B[fh];W[fj];B[gd];W[ra];B[dp]
                ;W[cp];B[go];W[gm];B[fm];W[sd];B[se];W[ho];B[hm];W[hn];B[ep];W[eq];B[cd]
                ;W[ei];B[dn];W[gp];B[pi];W[pf];B[dm];W[cm];B[je];W[jd];B[if];W[ie];B[ko]
                ;W[jo];B[je];W[kf];B[ni];W[dh];B[ge];W[ie];B[rg];W[je]N[Figure 4])
                
                (;B[dk]FG[257:Dia. 6]MN[1];W[ck];B[gk]N[Diagram 6]))
                
                (;B[nq]VW[ai:ss]FG[257:Dia. 5]MN[1];W[mr];B[nr];W[lr]TR[oq]N[Diagram 5]))
                
                (;B[mp]VW[ai:ss]FG[257:Dia. 4]MN[1];W[op];B[oo];W[no];B[mo];W[on];B[po]
                ;W[mn];B[np];W[nn];B[or]N[Diagram 4]))
                
                (;B[rc]VW[aa:sj]FG[257:Dia. 2]MN[1];W[rb];B[sb];W[la];B[ma];W[na];B[ja]
                ;W[pa]N[Diagram 2])
                
                (;B[rb]VW[aa:sj]FG[257:Dia. 3]MN[1];W[rc];B[sc];W[qb];B[pa];W[sb];B[sa]
                ;W[sd];B[qa]N[Diagram 3]))
                
                (;B[qf]VW[aa:sj]FG[257:Dia. 1]MN[1];W[mb];B[kc];W[qe];B[ne];W[kb];B[md]
                ;W[la];B[nb];W[eb]LB[ob:a][na:b][rc:c][sd:d]N[Diagram 1]))
                "#)?;
        
        Ok(())
    }
}
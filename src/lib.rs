mod builder;
mod parser;

use std::borrow::Cow;

pub use builder::build;
pub use builder::Builder;
pub use parser::parse;

use serde::{Deserialize, Serialize};

use strum::EnumDiscriminants;
use strum::EnumMessage;
use thiserror::Error;

#[derive(Error, Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum SgfToolError {
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

impl Point<'_> {
    pub fn xy(&self) -> (usize, usize) {
        let position = self.0.to_lowercase();
        let x = position.chars().nth(0).unwrap_or_default();
        let y = position.chars().nth(1).unwrap_or_default();

        (x as usize - 'a' as usize, y as usize - 'a' as usize)
    }

    pub fn xy_for_human(&self) -> (usize, usize) {
        let (x, y) = self.xy();
        (x + 1, y + 1)
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum Move<'a> {
    Move(#[serde(borrow)] Point<'a>),
    Pass,
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct PointRange<'a>(#[serde(borrow)] pub Point<'a>, pub Point<'a>);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Figure<'a>(pub usize, pub &'a str);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct PointText<'a>(pub Point<'a>, pub &'a str);

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub enum Player {
    Black,
    White,
}

#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
pub struct Base<'a> {
    #[serde(borrow)]
    pub tokens: Vec<Cow<'a, Token<'a>>>,
}

impl<'a> Base<'a> {
    pub fn add_token(&mut self, token: Token<'a>) {
        self.tokens.push(Cow::Owned(token));
    }

    pub fn get(&self, token_type: TokenType) -> Option<&Cow<'a, Token<'a>>> {
        self.tokens
            .iter()
            .find(|item| token_type == item.as_ref().into())
    }

    pub fn get_list(&self, token_type: TokenType) -> Vec<&Cow<'a, Token<'a>>> {
        self.tokens
            .iter()
            .filter(|item| token_type == item.as_ref().into())
            .collect::<Vec<_>>()
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, EnumMessage, EnumDiscriminants)]
#[strum_discriminants(name(TokenType))]
pub enum Token<'a> {
    Unknown(&'a str),

    /// Property: AP
    #[strum(message = "AP")]
    Application(&'a str),

    /// Property: C
    #[strum(message = "C")]
    Comment(&'a str),

    /// Property: CP
    #[strum(message = "CP")]
    Copyright(&'a str),

    /// Property: PB
    #[strum(message = "PB")]
    BlackName(&'a str),

    /// Property: PW
    #[strum(message = "PW")]
    WhiteName(&'a str),

    /// Property: BT
    #[strum(message = "BT")]
    BlackTeam(&'a str),

    /// Property: WT
    #[strum(message = "WT")]
    WhiteTeam(&'a str),

    /// Property: SZ
    #[strum(message = "SZ")]
    BoardSize(usize, usize),

    /// Variation
    Variation(Base<'a>),

    /// Property: FF
    #[strum(message = "FF")]
    FileFormat(usize),

    /// Property: GM
    #[strum(message = "GM")]
    GameType(usize),

    /// Property: CA
    #[strum(message = "CA")]
    Charset(&'a str),

    /// Property: ST
    #[strum(message = "ST")]
    VariationShown(usize),

    /// Property: PL
    #[strum(message = "PL")]
    WhoseTurn(Player),

    /// Property: AB
    #[strum(message = "AB")]
    BlackStones(Vec<Point<'a>>),

    /// Property: AW
    #[strum(message = "AW")]
    WhiteStones(Vec<Point<'a>>),

    /// Property: B
    #[strum(message = "B")]
    BlackMove(Move<'a>),

    /// Property: W
    #[strum(message = "W")]
    WhiteMove(Move<'a>),

    /// Property: BR
    #[strum(message = "BR")]
    BlackPlayerRank(&'a str),

    /// Property: WR
    #[strum(message = "WR")]
    WhitePlayerRank(&'a str),

    /// Property: SO
    #[strum(message = "SO")]
    Source(&'a str),

    /// Property: GN
    #[strum(message = "GN")]
    GameName(&'a str),

    /// Property: N
    #[strum(message = "N")]
    NodeName(&'a str),

    /// Property: RU
    #[strum(message = "RU")]
    Rule(&'a str),

    /// Property: KM
    #[strum(message = "KM")]
    Komi(f32),

    /// Property: AN
    #[strum(message = "AN")]
    PersonWhoProvidesAnnotations(&'a str),

    /// Property: AR
    #[strum(message = "AR")]
    DrawArrow(PointRange<'a>),

    /// Property: CR
    #[strum(message = "CR")]
    DrawCircle(Vec<Point<'a>>),

    /// Property: SQ
    #[strum(message = "SQ")]
    DrawSquare(Vec<Point<'a>>),

    /// Property: TR
    #[strum(message = "TR")]
    DrawTriangle(Vec<Point<'a>>),

    /// Property: DD
    #[strum(message = "DD")]
    GreyOut(Vec<Point<'a>>),

    /// Property: MA
    #[strum(message = "MA")]
    MarkX(Vec<Point<'a>>),

    /// Property: HA
    #[strum(message = "HA")]
    Handicap(usize),

    /// Property: RE
    #[strum(message = "RE")]
    Result(&'a str),

    /// Property: FG
    #[strum(message = "FG")]
    Figure(Option<Figure<'a>>),

    /// Property: PM
    #[strum(message = "PM")]
    Printing(usize),

    /// Property: TM
    #[strum(message = "TM")]
    TimeLimit(usize),

    /// Property: DT
    #[strum(message = "DT")]
    Date(&'a str),

    /// Property: AV
    #[strum(message = "AV")]
    Event(&'a str),

    /// Property: LB
    #[strum(message = "LB")]
    PointText(Vec<PointText<'a>>),

    /// Property: RO
    #[strum(message = "RO")]
    Round(&'a str),

    /// Property: US
    #[strum(message = "US")]
    SGFCreator(&'a str),

    /// Property: VW
    #[strum(message = "VW")]
    ViewOnly(Vec<PointRange<'a>>),

    /// Property: MN
    #[strum(message = "MN")]
    MoveNumber(usize),
}

// More detail https://homepages.cwi.nl/~aeb/go/misc/sgf.html#GM
// https://red-bean.com/sgf/properties.html#CR

#[cfg(test)]
mod tests {
    use crate::builder::Builder;
    use crate::parser::parse;
    use crate::*;

    #[test]
    fn basic_sgf_parse() -> Result<(), SgfToolError> {
        let result = parse("()")?;
        assert_eq!(result.tokens.len(), 0);
        assert_eq!(parse("(a)"), Err(SgfToolError::SyntaxIssue));
        assert_eq!(parse("(1)"), Err(SgfToolError::SyntaxIssue));
        assert_eq!(parse("("), Err(SgfToolError::SyntaxIssue));
        assert_eq!(parse(")"), Err(SgfToolError::SyntaxIssue));
        assert_eq!(parse(""), Err(SgfToolError::SyntaxIssue));
        assert_eq!(parse("-"), Err(SgfToolError::SyntaxIssue));
        assert_eq!(parse(" "), Err(SgfToolError::SyntaxIssue));
        Ok(())
    }

    #[test]
    fn sgf_parse() -> Result<(), SgfToolError> {
        let result = parse(
            r#"(
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
)"#,
        )?;
        assert_eq!(result.tokens.len(), 4);
        assert_eq!(result.tokens[0].as_ref(), &Token::FileFormat(4));
        assert_eq!(result.tokens[1].as_ref(), &Token::Comment("root"));

        if let Token::Variation(trees) = result.tokens[2].as_ref() {
            assert_eq!(trees.tokens.len(), 4);
            assert_eq!(trees.tokens[0].as_ref(), &Token::Comment("a"));
            assert_eq!(trees.tokens[1].as_ref(), &Token::Comment("b"));

            if let Token::Variation(trees) = trees.tokens[2].as_ref() {
                assert_eq!(trees.tokens.len(), 1);
                assert_eq!(trees.tokens[0].as_ref(), &Token::Comment("c"));
            } else {
                assert!(false, "Variation not found");
            }

            if let Token::Variation(trees) = trees.tokens[3].as_ref() {
                assert_eq!(trees.tokens.len(), 2);
                assert_eq!(trees.tokens[0].as_ref(), &Token::Comment("d"));
                assert_eq!(trees.tokens[1].as_ref(), &Token::Comment("e"));
            } else {
                assert!(false, "Variation not found");
            }
        } else {
            assert!(false, "Variation not found");
        }

        if let Token::Variation(trees) = &result.tokens[3].as_ref() {
            assert_eq!(trees.tokens.len(), 3);
            assert_eq!(trees.tokens[0].as_ref(), &Token::Comment("f"));

            if let Token::Variation(trees) = trees.tokens[1].as_ref() {
                assert_eq!(trees.tokens.len(), 3);
                assert_eq!(trees.tokens[0].as_ref(), &Token::Comment("g"));
                assert_eq!(trees.tokens[1].as_ref(), &Token::Comment("h"));
                assert_eq!(trees.tokens[2].as_ref(), &Token::Comment("i"));
            } else {
                assert!(false, "Variation not found");
            }

            if let Token::Variation(trees) = trees.tokens[2].as_ref() {
                assert_eq!(trees.tokens.len(), 1);
                assert_eq!(trees.tokens[0].as_ref(), &Token::Comment("j"));
            } else {
                assert!(false, "Variation not found");
            }
        } else {
            assert!(false, "Variation not found");
        }

        parse(
            r#"
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
             "#,
        )?;

        parse(
            r#"(;FF[4]GM[1]SZ[19]FG[257:Figure 1]PM[2]
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
                "#,
        )?;

        Ok(())
    }

    #[test]
    fn basic_test_1() -> Result<(), SgfToolError> {
        let source = "(;C[Black to play and win, Igo Hatsuyo-ron Problem 120];AB[ra][hb][lb][fc][lc][bd][ld][ce][de][fe][le][me][oe][pe][bf][mf][of][og][dh][oh][ph][qh][rh][sh][di][mi][ni][oi][pi][aj][fj][lj][ak][ek][lk][rk][sk][al][el][il][pl][ql][am][bm][em][qm][rm][dn][fn][mn][co][fo][ko][oo][bp][cp][ep][pp][sp][fq][pq][qq][sq][cr][nr][pr][bs][ns][os][ps];AW[qa][ib][jb][mb][rb][hc][qc][cd][jd][nd][od][ke][qe][re][df][ff][pf][bg][cg][dg][gg][hg][kg][lg][ng][ah][hi][ki][ri][si][bj][cj][jj][nj][oj][pj][qj][dk][jk][ok][dl][jl][rl][sl][hm][jm][sm][bn][cn][en][jn][on][rn][sn][qo][ro][so][ap][hp][kp][lp][mp][op][qp][eq][rq][ar][ir][mr][or][ms])";
        let mut buffer = String::new();
        let tree = parse(&source)?;
        tree.build(&mut buffer)?;
        assert_eq!(buffer, source);
        Ok(())
    }

    #[test]
    fn basic_test_2() -> Result<(), SgfToolError> {
        let source = "(;AW[ca][cb][cc][bd][cd];AB[da][eb][dc][ec][dd][fd][be][ce][de];B[ab];W[bb];B[ac];W[ad];B[aa];C[RIGHT])";
        let mut buffer = String::new();
        let tree = parse(&source)?;
        tree.build(&mut buffer)?;
        assert_eq!(buffer, source);
        Ok(())
    }

    #[test]
    fn basic_test_3() -> Result<(), SgfToolError> {
        let source = "(;AW[hh][lh][hi][ji][li][lj];AB[kg][lg][mg][mh][mi][mj][kk][lk][mk];C[Black to play and catch the three stones.](;B[ki];W[kh](;B[jh];W[kj];B[jj];W[ki];B[ii];C[RIGHT])(;B[kj];W[jh]))(;B[jh];W[jj])(;B[jj];W[jh])(;B[ii];W[jj]))";
        let mut buffer = String::new();
        let tree = parse(&source)?;
        tree.build(&mut buffer)?;
        assert_eq!(buffer, source);
        Ok(())
    }

    #[test]
    fn basic_test_4() -> Result<(), SgfToolError> {
        let source = "(;FF[4];C[root];SZ[19];B[aa];W[ab];B[])";
        let mut buffer = String::new();
        let tree = parse(&source)?;
        tree.build(&mut buffer)?;
        assert_eq!(buffer, source);
        Ok(())
    }

    #[test]
    fn basic_test_5() -> Result<(), SgfToolError> {
        let point = Point("ss");
        assert_eq!(point.xy(), (18, 18));
        assert_eq!(point.xy_for_human(), (19, 19));
        Ok(())
    }

    #[test]
    fn basic_test_6() -> Result<(), SgfToolError> {
        let source = "(;FF[4];C[root];SZ[19];B[aa];W[ab];B[])";
        let tree = parse(&source)?;

        assert_eq!(
            tree.get(TokenType::FileFormat),
            Some(Cow::Owned(Token::FileFormat(4))).as_ref()
        );
        assert_eq!(
            tree.get(TokenType::Comment),
            Some(Cow::Owned(Token::Comment("root"))).as_ref()
        );
        assert_eq!(
            tree.get(TokenType::BoardSize),
            Some(Cow::Owned(Token::BoardSize(19, 19))).as_ref()
        );
        assert_eq!(
            tree.get(TokenType::BlackMove),
            Some(Cow::Owned(Token::BlackMove(Move::Move(Point("aa"))))).as_ref()
        );

        let items = tree.get_list(TokenType::BlackMove);
        assert_eq!(items.len(), 2);
        assert_eq!(
            items.get(0),
            Some(Cow::Owned(Token::BlackMove(Move::Move(Point("aa")))))
                .as_ref()
                .as_ref()
        );
        assert_eq!(
            items.get(1),
            Some(Cow::Owned(Token::BlackMove(Move::Pass)))
                .as_ref()
                .as_ref()
        );
        Ok(())
    }
}

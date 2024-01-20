# SGF-TOOL

![Build & Test](https://github.com/erhanbaris/sgf-tool/actions/workflows/rust.yml/badge.svg)
[![Latest Version](https://img.shields.io/crates/v/sgf-tool.svg)](https://crates.io/crates/sgf-tool)
[![Rust Documentation](https://docs.rs/sgf-tool/badge.svg)](https://docs.rs/sgf-tool)
![Crates.io](https://img.shields.io/crates/l/sgf-tool)
![Crates.io](https://img.shields.io/crates/d/sgf-tool)

SGF file format parser and builder.

Reference: https://red-bean.com/sgf/sgf4.html

## Examples

```rust
use std::borrow::Cow;
use sgf_tool::*;

fn main() -> Result<(), SgfToolError> {
    // Parse sgf
    let source = "(;FF[4];C[root];SZ[19];B[aa];W[ab])";
    let base = parse(&source)?;

    for token in base.tokens.iter() {
        println!("Token: {:?}", token);
    }

    // Or

    assert_eq!(tree.get(TokenType::FileFormat), Some(Cow::Owned(Token::FileFormat(4))).as_ref());

    /* Rebuild sgf and validage */
    let mut buffer = String::new();
    base.build(&mut buffer)?;
    assert_eq!(buffer, source);

    // Build sgf
    let mut base = sgf_tool::Base::default();
    base.add_token(Token::Application("sgf-tool"));
    base.add_token(Token::BlackMove(Some(Point("ab"))));
    base.add_token(Token::WhiteMove(Some(Point("bc"))));
    base.add_token(Token::BlackMove(None));

    assert_eq!("(;AP[sgf-tool];B[ab];W[bc];B[])", &build(base)?);
    Ok(())
}
```

# SGF-TOOL

SGF file format parser and builder.

## Examples

```rust
use sgf_tool::*;

fn main() -> Result<(), SgfToolError> {
    // Parse sgf
    let source = "(;FF[4];C[root];SZ[19];B[aa];W[ab])";
    let base = parse(&source)?;

    for token in base.tokens.iter() {
        println!("Token: {:?}", token);
    }

    /* Rebuild sgf and validage */
    let mut buffer = String::new();
    base.build(&mut buffer)?;
    assert_eq!(buffer, source);

    // Build sgf
    let mut base = sgf_tool::Base::default();
    base.add_token(Token::Application("sgf-tool"));
    base.add_token(Token::BlackMove(Point("ab")));
    base.add_token(Token::WhiteMove(Point("bc")));

    assert_eq!("(;AP[sgf-tool];B[ab];W[bc])", &build(base)?);
    Ok(())
}
```

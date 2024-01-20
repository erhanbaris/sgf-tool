# SGF-TOOL

SGF file format parser and builder.

## Examples

```rust
use sgf_tool::*;

fn main() -> Result<(), SgfToolError> {
    let source = "(;FF[4];C[root];SZ[19];B[aa];W[ab])";
    let tree = parse(&source)?;

    let mut buffer = String::new();
    tree.build(&mut buffer)?;

    assert_eq!(buffer, source);
    Ok(())
}
```

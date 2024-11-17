fn main() {
    let s = "ld $500, r56";
    let parsed = sm213_parser::parse(s).unwrap();
    dbg!(&parsed);
}

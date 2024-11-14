// use chumsky::span::SimpleSpan;
use parser::span::Span;
// pub use syntax;

// #[derive(Debug, Clone)]
// pub enum ParsingError {
//     InvalidRegister {
//         span: SimpleSpan,
//     },
//     IntegerOverflow {
//         span: SimpleSpan,
//         typ: NumType,
//     },
//     InvalidSyscallCode {
//         code: Num,
//     },
//     InvalidPosLocation {
//         span: SimpleSpan,
//         loc: i32,
//     },
//     InvalidOffset {
//         expected_range: RangeInclusive<i32>,
//         expected_divisor: u8,
//         found: Num,
//     },
//     InvalidShiftAmount {
//         expected_range: RangeInclusive<i32>,
//         found: Num,
//     },
//     InvalidIndexMultiple {
//         span: SimpleSpan,
//         found: i32,
//     },
//     UnknownToken {
//         pos: usize,
//     },
//     ExpectedOtherToken {
//         expected: &'static [&'static str],
//         // found: TokenType,
//         pos: usize,
//     },
// }

// static ZERONUM: LazyLock<Num> = LazyLock::new(|| Num {
//     kind: NumType::Decimal,
//     value: 0,
//     span: (0..0).into(),
// });

// reg type, used similarly to a primitive
// val must be 0 <= val <= 7
#[derive(Clone, Copy)]
pub struct Reg {
    pub inner: u8,
    pub span: Span,
}

impl std::fmt::Debug for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.inner)
    }
}

#[derive(Debug, Clone)]
pub struct InstructionWithSpan<'source> {
    inst: Instruction<'source>,
    span: Span,
}

impl<'source> InstructionWithSpan<'source> {
    pub fn new(inst: Instruction<'source>, span: Span) -> Self {
        Self { inst, span }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Label<'source>(pub &'source str, pub Span);

#[derive(Debug, Clone, Copy)]
pub enum LoadFrom<'source> {
    ImmediateNumber(Num),
    ImmediateLabel(Label<'source>),
    // offset must be a multiple of 4, and must be 0 <= offset <= 60
    // as it is represented as a hexit
    Offset { offset: Option<Num>, base: Reg },
    Indexed { base: Reg, index: Reg },
}

#[derive(Debug, Clone, Copy)]
pub enum StoreTo {
    // offset must be a multiple of 4, and must be 0 <= offset <= 60
    // as it is represented as a hexit
    Offset { offset: Option<Num>, base: Reg },
    Indexed { base: Reg, index: Reg },
}

#[derive(Debug, Clone, Copy)]
pub enum BranchLocation<'source> {
    Address(Num),
    Label(Label<'source>),
}

#[derive(Debug, Clone, Copy)]
pub enum DirectiveLongValue<'source> {
    Number(Num),
    Label(Label<'source>),
}

#[derive(Debug, Clone, Copy)]
pub enum JumpLocation<'source> {
    Label(Label<'source>),
    // Must NOT be negative
    Addr(Num),
    // offset must be divisible by 2 and 0 <= 508 as it is stored in a single byte * 2
    Indirect { offset: Option<Num>, to: Reg },
    DoubleIndirect(DoubleIndirectMethod),
}

#[derive(Debug, Clone, Copy)]
pub enum DoubleIndirectMethod {
    // offset must be divisible by 4 and 0 <= 1020 as it is stored in a single byte * 4
    Offset { offset: Option<Num>, to: Reg },
    Indexed { base: Reg, index: Reg },
}

#[rustfmt::skip]
#[derive(Debug, Clone)]
pub enum Instruction<'source> {
    Load { from: (LoadFrom<'source>, Span), to: Reg },
    Store { from: Reg, to: (StoreTo, Span) },
    Halt,
    Nop,
    Mov { from: Reg, to: Reg },
    Add { from: Reg, to: Reg },
    And { from: Reg, to: Reg },
    Inc { reg: Reg },
    IncAddr { reg: Reg },
    Dec { reg: Reg },
    DecAddr { reg: Reg },
    Not { reg: Reg },
    // amt must be 0 <= amt <= 127
    // Note: The amt bound for this is intentionally different from ShiftRight
    ShiftLeft { amt: Num, reg: Reg },
    // amt must be 0 <= amt <= 128
    // Note: The amt bound for this is intentionally different from ShiftLeft
    ShiftRight { amt: Num, reg: Reg },
    Branch { to: (BranchLocation<'source>, Span) },
    BranchIfEqual { reg: Reg, to: (BranchLocation<'source>, Span) },
    BranchIfGreater { reg: Reg, to: (BranchLocation<'source>, Span) },
    // offset must be 0 <= offset <= 30 as it is stored in a hexit * 2
    GetProgramCounter { offset: Num, to: Reg },
    // Must NOT be negative
    Jump {
        to: (JumpLocation<'source>, Span),
    },
    Syscall { typ: (SyscallType, Span) },

    DirectiveLong { value: (DirectiveLongValue<'source>, Span) },
    // loc must not be negative
    DirectivePos { loc: (Num, Span) },
    // Label { name: String },
}

#[derive(Clone, Copy)]
pub struct Num {
    pub kind: NumType,
    pub value: i32,
    pub span: Span,
}

#[derive(Clone, Debug, Copy)]
pub enum NumType {
    Hexadecimal,
    Decimal,
}

impl std::fmt::Debug for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            NumType::Hexadecimal => write!(f, "Hex(0x{:x})", self.value),
            NumType::Decimal => write!(f, "Dec({})", self.value),
        }
    }
}

impl std::fmt::Display for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            NumType::Hexadecimal => write!(f, "0x{:x}", self.value),
            NumType::Decimal => write!(f, "{}", self.value),
        }
    }
}

impl PartialEq<i32> for Num {
    fn eq(&self, other: &i32) -> bool {
        self.value.eq(other)
    }
}
impl PartialEq for Num {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}
impl PartialOrd<i32> for Num {
    fn partial_cmp(&self, other: &i32) -> Option<std::cmp::Ordering> {
        self.value.partial_cmp(other)
    }
}
impl PartialEq<Num> for i32 {
    fn eq(&self, other: &Num) -> bool {
        self.eq(&other.value)
    }
}
impl PartialOrd<Num> for i32 {
    fn partial_cmp(&self, other: &Num) -> Option<std::cmp::Ordering> {
        self.partial_cmp(&other.value)
    }
}
impl std::ops::Rem<i32> for Num {
    type Output = i32;
    fn rem(self, rhs: i32) -> Self::Output {
        self.value.rem(rhs)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SyscallType {
    Read,
    Write,
    Exec,
    // Invalid code found
    Err,
}
#[derive(Debug, Clone)]
pub struct Program<'source> {
    pub inner: Vec<Line<'source>>,
}

// #[derive(Debug, Clone)]
// pub struct LineWithSpan<'source> {
//     pub line: Line<'source>,
//     pub span: Span,
// }

#[derive(Debug, Clone)]
pub struct Statement<'source> {
    // This exists so we can format and keep comment locations.
    label_and_comment: Option<(Label<'source>, Vec<&'source str>)>,
    instruction: InstructionWithSpan<'source>,
}

impl<'source> Statement<'source> {
    pub const fn new(
        label: Option<(Label<'source>, Vec<&'source str>)>,
        inner: InstructionWithSpan<'source>,
    ) -> Self {
        Self {
            label_and_comment: label,
            instruction: inner,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Line<'source> {
    Code(Statement<'source>),
    Comment(&'source str),
    CodeAndComment(Statement<'source>, &'source str),
    // Label(&'source str),
    // LabelAndComment(&'source str, &'source str),
    Empty,
}

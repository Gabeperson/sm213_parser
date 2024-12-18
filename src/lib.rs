use std::collections::HashMap;

pub use parser;
use parser::prelude::*;

#[allow(clippy::too_many_lines)]
pub fn parse(input: &str) -> Result<Program, ParseError> {
    // pub fn parse(input: &str) -> Result<InstructionWithSpan, ParseError> {
    let ws0 = choice((" ", "\t")).repeated().ignored();

    let ws1 = choice((" ", "\t"))
        .repeated()
        .at_least(1)
        .labelled("whitespace")
        .ignored()
        .cut();
    let nl = choice(("\r\n", "\r", "\n")).labelled("newline").ignored();

    let nl_or_eof = EndOfInput.or(nl);
    let comment = ws0
        .then("#")
        .then(Any1.and_is(nl_or_eof.not()).repeated())
        .slice();

    let comma = ",".padded_by(ws0).ignored();

    // let open_paren = "(".padded_by(ws0).ignored();

    // let close_paren = ")".padded_by(ws0).ignored();
    let label = (Alpha.or("_").labelled("label"))
        .then(AlphaNumeric.or("_").repeated())
        .slice()
        .map_with_span(Label)
        .try_map(|label| match label.0 {
            "r0" | "r1" | "r2" | "r3" | "r4" | "r5" | "r6" | "r7" => Err(ParseError {
                message: ErrorMessage::Custom(format!(
                    "r[0-7] cannot be a label, but found {}",
                    label.0
                )),
                span_or_pos: SpanOrPos::Span(label.1),
                kind: ParseErrorType::Cut,
            }),
            _ => Ok(label),
        });
    let labelmarker = label.then_ignore(ws0.then(":"));

    let mult = int(36)
        .try_map_with_span(|s, span| {
            if s != "4" {
                return Err(ParseError {
                    message: ErrorMessage::Custom(format!("Multiplier must be 4, but found {s}")),
                    span_or_pos: SpanOrPos::Span(span),
                    kind: ParseErrorType::Cut,
                });
            }
            Ok(())
        })
        .ignored();
    let reg = "r"
        .labelled("register")
        // parse all alphanumerics for better errors
        .ignore_then(int(36).labelled("register number (0-7)").cut())
        .try_map_with_span(|s, span| {
            let inner = match s.parse::<u8>() {
                Ok(n) if n == 0 => n,
                Ok(n) if n <= 7 => {
                    if s.starts_with("0") {
                        return Err(ParseError {
                            message: ErrorMessage::Custom(format!(
                                "Registers should not have extra '0's in front, found {s}"
                            )),
                            span_or_pos: SpanOrPos::Span(span),
                            kind: ParseErrorType::Cut,
                        });
                    }
                    n
                }
                _ => {
                    return Err(ParseError {
                        message: ErrorMessage::Custom(format!("Expected registers 0-7, found {s}")),
                        span_or_pos: SpanOrPos::Span(span),
                        kind: ParseErrorType::Cut,
                    })
                }
            };
            Ok(Reg { inner, span })
        });
    let hex =
        "0x".labelled("hexadecimal number")
            // be lenient for better errors
            .ignore_then(int(36).cut())
            .try_map_with_span(|s, span| {
                #[allow(clippy::cast_possible_wrap)]
                u32::from_str_radix(s, 16)
                    .map_err(|e| match e.kind() {
                        std::num::IntErrorKind::InvalidDigit => ParseError {
                            message: ErrorMessage::Custom(format!(
                                "Couldn't parse hexadecimal integer: `0x{s}`. Invalid digits found"
                            )),
                            span_or_pos: SpanOrPos::Span(span),
                            kind: ParseErrorType::Cut,
                        },
                        std::num::IntErrorKind::PosOverflow
                        | std::num::IntErrorKind::NegOverflow => ParseError {
                            message: ErrorMessage::Custom(format!(
                                "Couldn't parse hexadecimal integer: `0x{s}`. Overflow occured"
                            )),
                            span_or_pos: SpanOrPos::Span(span),
                            kind: ParseErrorType::Cut,
                        },
                        _ => unreachable!(),
                    })
                    .map(|n| Num {
                        kind: NumType::Hexadecimal,
                        value: n as i32,
                        span,
                    })
            });
    let deci =
        "-".optional()
            .then(int(36))
            .slice()
            .try_map_with_span(|s, span| {
                #[allow(clippy::cast_possible_wrap)]
                s.parse::<i32>()
                    .map_err(|e| match e.kind() {
                        std::num::IntErrorKind::InvalidDigit => ParseError {
                            message: ErrorMessage::Custom(format!(
                                "Couldn't parse decimal integer: `{s}`. Invalid digits found"
                            )),
                            span_or_pos: SpanOrPos::Span(span),
                            kind: ParseErrorType::Cut,
                        },
                        std::num::IntErrorKind::PosOverflow
                        | std::num::IntErrorKind::NegOverflow => ParseError {
                            message: ErrorMessage::Custom(format!(
                                "Couldn't parse decimal integer: `0x{s}`. Overflow occured"
                            )),
                            span_or_pos: SpanOrPos::Span(span),
                            kind: ParseErrorType::Cut,
                        },
                        _ => unreachable!(),
                    })
                    .map(|n| Num {
                        kind: NumType::Decimal,
                        value: n,
                        span,
                    })
            });
    let integer = hex.or(deci).labelled("hex or dec integer");
    let immediate_num = ("$".then(ws0)).ignore_then(integer);

    let base_plus_offset = integer
        .optional()
        .then_ignore(ws0)
        .then(reg.padded_by(ws0).delimited_by("(", ")"))
        .boxed();
    let indexed = group((
        reg.padded_by(ws0),
        comma,
        reg.padded_by(ws0),
        comma,
        mult.padded_by(ws0),
    ))
    .delimited_by("(", ")")
    .map(|(base, (), index, (), ())| (base, index))
    .boxed();
    let ld_base_plus_offset = base_plus_offset
        .clone()
        .try_map_with_span(|(offset, base), span| {
            if let Some(offset) = offset {
                if !((0..=60).contains(&offset) && offset % 4 == 0) {
                    return Err(ParseError {
                        message: ErrorMessage::Custom(format!(
                        "Offset should be 0 <= offset <= 60 and divisible by 4, found `{offset}`"
                    )),

                        span_or_pos: SpanOrPos::Span(offset.span),
                        kind: ParseErrorType::Cut,
                    });
                }
            }
            Ok((LoadFrom::Offset { offset, base }, span))
        });
    let ld_indexed = indexed
        .clone()
        .map_with_span(|(base, index), span| (LoadFrom::Indexed { base, index }, span));

    let ld = group((
        "ld",
        ws1,
        choice((
            (("$".then(ws0)).ignore_then(
                label
                    .map_with_span(|label, span| (LoadFrom::ImmediateLabel(label), span))
                    .or(integer.map_with_span(|int, span| (LoadFrom::ImmediateNumber(int), span)))
                    .if_no_progress(ErrorMessage::ExpectedOtherToken {
                        expected: vec!["label".to_string(), "integer".to_string()],
                    }),
            )),
            ld_base_plus_offset,
            ld_indexed,
        ))
        .if_no_progress(ErrorMessage::ExpectedOtherToken {
            expected: vec![
                "immediate number".to_string(),
                "immediate_label".to_string(),
                "offset access".to_string(),
                "indexed access".to_string(),
            ],
        })
        .cut(),
        comma.cut(),
        reg.cut(),
    ))
    .map(|(_, (), from, (), to)| (from, to))
    .map_with_span(|(from, reg), span| {
        InstructionWithSpan::new(Instruction::Load { from, to: reg }, span)
    })
    .boxed();
    let st_base_plus_offset = base_plus_offset
        .clone()
        .try_map_with_span(|(offset, base), span| {
            if let Some(offset) = offset {
                if !((0..=60).contains(&offset) && offset % 4 == 0) {
                    return Err(ParseError {
                        message: ErrorMessage::Custom(format!(
                        "Offset should be 0 <= offset <= 60 and divisible by 4, found `{offset}`"
                    )),

                        span_or_pos: SpanOrPos::Span(offset.span),
                        kind: ParseErrorType::Cut,
                    });
                }
            };
            Ok((StoreTo::Offset { offset, base }, span))
        });
    let st_indexed = indexed
        .clone()
        .map_with_span(|(base, index), span| (StoreTo::Indexed { base, index }, span));
    let st = "st"
        .ignore_then(ws1)
        .ignore_then(reg.cut())
        .then_ignore(comma.cut())
        .then(
            st_base_plus_offset
                .or(st_indexed)
                .if_no_progress(ErrorMessage::ExpectedOtherToken {
                    expected: vec!["offset access".to_string(), "indexed access".to_string()],
                })
                .cut(),
        )
        .map_with_span(|(reg, to), span| {
            InstructionWithSpan::new(Instruction::Store { from: reg, to }, span)
        })
        .boxed();
    let jmp_location = choice((
        base_plus_offset.clone().try_map_with_span(|(offset, base), span| {
            if let Some(offset) = offset {
                if !((0..=510).contains(&offset) && offset % 2 == 0) {
                    return Err(ParseError {
                        message: ErrorMessage::Custom(format!(
                            "jmp offset should be 0 <= offset <= 510 and divisible by 2, found `{offset}`"
                        )),
                        span_or_pos: SpanOrPos::Span(span),
                        kind: ParseErrorType::Cut,
                    });
                }
                }
            Ok((JumpLocation::Indirect { offset, to: base }, span))
        }),
        label.map_with_span(|to, span| {
            (JumpLocation::Label(to), span)
        }),
        integer.try_map_with_span(|to, span| {
            if to < 0 {
                return Err(ParseError {
                    message: ErrorMessage::Custom(format!(
                        "jump position should be positive, found `{to}`"
                    )),
                    span_or_pos: SpanOrPos::Span(span),
                    kind: ParseErrorType::Cut,
                });
            }
            Ok((JumpLocation::Addr(to), span))
        }),
        "*".ignore_then(
            base_plus_offset.clone().try_map_with_span(|(offset, base), span| {
                if let Some(offset) = offset {
                    if !((0..=1020).contains(&offset) && offset % 4 == 0) {
                        return Err(ParseError {
                            message: ErrorMessage::Custom(format!(
                                "jmp offset should be 0 <= offset <= 1020 and divisible by 4, found `{offset}`"
                            )),
                            span_or_pos: SpanOrPos::Span(span),
                            kind: ParseErrorType::Cut,
                        });
                    }
                }
                Ok(DoubleIndirectMethod::Offset { offset, to: base })
            }).or(indexed.map(|(base, index)| {
                DoubleIndirectMethod::Indexed { base , index  }
            }))
            .map_with_span(|method, span| {
                (JumpLocation::DoubleIndirect(method), span)
            })
        ),
    )).if_no_progress(
        ErrorMessage::ExpectedOtherToken {
            expected:
                vec![
                    "address".to_string(),
                    "label".to_string(),
                    "indirect access".to_string(),
                    "double indirect access".to_string()
                ]
            }
        ).cut().boxed();
    let halt = "halt"
        .to_span()
        .map(|sp| InstructionWithSpan::new(Instruction::Halt, sp));
    let nop = "nop"
        .to_span()
        .map(|sp| InstructionWithSpan::new(Instruction::Nop, sp));
    let mov = "mov"
        .then(ws1)
        .ignore_then(reg.cut())
        .then_ignore(comma.cut())
        .then(reg.cut())
        .map_with_span(|(from, to), span| {
            InstructionWithSpan::new(Instruction::Mov { from, to }, span)
        })
        .boxed();
    let add = "add"
        .then(ws1)
        .ignore_then(reg.cut())
        .then_ignore(comma.cut())
        .then(reg.cut())
        .map_with_span(|(from, to), span| {
            InstructionWithSpan::new(Instruction::Add { from, to }, span)
        })
        .boxed();
    let and = "and"
        .then(ws1)
        .ignore_then(reg.cut())
        .then_ignore(comma.cut())
        .then(reg.cut())
        .map_with_span(|(from, to), span| {
            InstructionWithSpan::new(Instruction::And { from, to }, span)
        })
        .boxed();
    let inc = "inc"
        .then(ws1)
        .ignore_then(reg.cut())
        .map_with_span(|reg, span| InstructionWithSpan::new(Instruction::Inc { reg }, span));
    let inca = "inca"
        .then(ws1)
        .ignore_then(reg.cut())
        .map_with_span(|reg, span| InstructionWithSpan::new(Instruction::IncAddr { reg }, span));
    let dec = "dec"
        .then(ws1)
        .ignore_then(reg.cut())
        .map_with_span(|reg, span| InstructionWithSpan::new(Instruction::Dec { reg }, span));
    let deca = "deca"
        .then(ws1)
        .ignore_then(reg.cut())
        .map_with_span(|reg, span| InstructionWithSpan::new(Instruction::DecAddr { reg }, span));
    let not = "not"
        .then(ws1)
        .ignore_then(reg.cut())
        .map_with_span(|reg, span| InstructionWithSpan::new(Instruction::Not { reg }, span));
    let shl = "shl"
        .then(ws1)
        .ignore_then(immediate_num.cut())
        .then_ignore(comma.cut())
        .then(reg.cut())
        .try_map_with_span(|(num, reg), span| {
            if !(0..=127).contains(&num) {
                return Err(ParseError {
                    message: ErrorMessage::Custom(format!(
                        "Shl amount should be 0 <= amt <= 127, found `{num}`"
                    )),
                    span_or_pos: SpanOrPos::Span(span),
                    kind: ParseErrorType::Cut,
                });
            }
            Ok(InstructionWithSpan::new(
                Instruction::ShiftLeft { amt: num, reg },
                span,
            ))
        })
        .boxed();
    let shr = "shr"
        .then(ws1)
        .ignore_then(immediate_num.cut())
        .then_ignore(comma.cut())
        .then(reg.cut())
        .try_map_with_span(|(num, reg), span| {
            if !(0..=128).contains(&num) {
                return Err(ParseError {
                    message: ErrorMessage::Custom(format!(
                        "Shl amount should be 0 <= amt <= 128, found `{num}`"
                    )),
                    span_or_pos: SpanOrPos::Span(span),
                    kind: ParseErrorType::Cut,
                });
            }
            Ok(InstructionWithSpan::new(
                Instruction::ShiftRight { amt: num, reg },
                span,
            ))
        })
        .boxed();
    let br_label = label.map_with_span(|to, span| (BranchLocation::Label(to), span));
    let br_addr = integer.try_map_with_span(|to, span| {
        if to < 0 {
            return Err(ParseError {
                message: ErrorMessage::Custom(format!(
                    "Absolute branch position should be positive, found `{to}`"
                )),
                span_or_pos: SpanOrPos::Span(span),
                kind: ParseErrorType::Cut,
            });
        }
        if to % 2 != 0 {
            return Err(ParseError {
                message: ErrorMessage::Custom(format!(
                    "Branch location should be divisible by 2, found `{to}`"
                )),
                span_or_pos: SpanOrPos::Span(span),
                kind: ParseErrorType::Cut,
            });
        }
        Ok((BranchLocation::Address(to), span))
    });
    let br = "br"
        .ignore_then(ws1)
        .ignore_then(
            br_label
                .or(br_addr)
                .or(expected(&["label", "address"]))
                .cut(),
        )
        .map_with_span(|to, span| InstructionWithSpan::new(Instruction::Branch { to }, span))
        .boxed();
    let beq = "beq"
        .ignore_then(ws1)
        .ignore_then(reg.cut())
        .then_ignore(comma.cut())
        .then(
            br_label
                .or(br_addr)
                .or(expected(&["label", "address"]))
                .cut(),
        )
        .map_with_span(|(reg, to), span| {
            InstructionWithSpan::new(Instruction::BranchIfEqual { reg, to }, span)
        })
        .boxed();
    // let bgt = bgt_label.or(bgt_addr);
    let bgt = "bgt"
        .ignore_then(ws1)
        .ignore_then(reg.cut())
        .then_ignore(comma.cut())
        .then(
            br_label
                .or(br_addr)
                .or(expected(&["label", "address"]))
                .cut(),
        )
        .map_with_span(|(reg, to), span| {
            InstructionWithSpan::new(Instruction::BranchIfGreater { reg, to }, span)
        })
        .boxed();

    let jmp = "j"
        .ignore_then(ws1)
        .ignore_then(jmp_location.cut())
        .map_with_span(|loc, span| InstructionWithSpan::new(Instruction::Jump { to: loc }, span))
        .boxed();
    let sys = "sys"
        .then(ws1)
        .ignore_then(immediate_num.cut())
        .try_map_with_span(|num, span| match num.value {
            0 => Ok((SyscallType::Read, span)),
            1 => Ok((SyscallType::Write, span)),
            2 => Ok((SyscallType::Exec, span)),
            other => Err(ParseError {
                message: ErrorMessage::Custom(format!(
                    "Found invalid Syscall code. Expected one of 0, 1, 2 but found {other}"
                )),
                span_or_pos: SpanOrPos::Span(span),
                kind: ParseErrorType::Cut,
            }),
        })
        .map_with_span(|code, span| {
            InstructionWithSpan::new(Instruction::Syscall { typ: code }, span)
        })
        .boxed();
    let gpc = "gpc"
        .then(ws1)
        .ignore_then(immediate_num.cut())
        .then_ignore(comma.cut())
        .then(reg.cut())
        .try_map_with_span(|(num, reg), span| {
            if !((0..=30).contains(&num) && num % 2 == 0) {
                return Err(ParseError {
                    message: ErrorMessage::Custom(format!(
                        "gpc offset should be 0 <= offset <= 30 and divisible by 2, found `{num}`"
                    )),
                    span_or_pos: SpanOrPos::Span(span),
                    kind: ParseErrorType::Cut,
                });
            }
            Ok(InstructionWithSpan::new(
                Instruction::GetProgramCounter {
                    offset: num,
                    to: reg,
                },
                span,
            ))
        })
        .boxed();
    let directive = "."
        .ignore_then(
            "long"
                .ignore_then(ws1)
                .ignore_then(
                    label
                        .map_with_span(|l, span| (DirectiveLongValue::Label(l), span))
                        .or(integer
                            .map_with_span(|num, span| (DirectiveLongValue::Number(num), span)))
                        .if_no_progress(ErrorMessage::ExpectedOtherToken {
                            expected: vec!["label".to_string(), "number".to_string()],
                        })
                        .cut()
                        .map(|val| Instruction::DirectiveLong { value: val }),
                )
                .or("pos"
                    .ignore_then(ws1)
                    .ignore_then(hex.cut().try_map_with_span(|hex, span| {
                        if hex.value < 0 {
                            return Err(ParseError {
                                message: ErrorMessage::Custom(format!(
                                    ".pos location must be positive, found {}",
                                    hex.value,
                                )),
                                span_or_pos: SpanOrPos::Span(span),
                                kind: ParseErrorType::Cut,
                            });
                        }
                        Ok((hex, span))
                    }))
                    .map(|pos| Instruction::DirectivePos { loc: pos }))
                .if_no_progress(ErrorMessage::ExpectedOtherToken {
                    expected: vec![r#""long""#.to_string(), r#""pos""#.to_string()],
                })
                .cut(),
        )
        .map_with_span(InstructionWithSpan::new)
        .boxed();

    let instruction = choice((
        ld, st, halt, nop, mov, add, and, inca, inc, deca, dec, not, shl, shr, br, beq, bgt, jmp,
        sys, directive, gpc,
    ))
    .boxed();

    let section_empty = ws0.then_ignore(nl).to(Line::Empty).boxed();
    let section_comment = comment.then_ignore(nl_or_eof).map(Line::Comment).boxed();
    let section_instruction_maybecomment = group((
        ws0,
        instruction
            .clone()
            .if_no_progress(ErrorMessage::ExpectedOtherToken {
                expected: vec!["instruction".to_string()],
            }),
        comment.optional(),
        ws0,
        nl_or_eof.labelled("end of line").cut(),
    ))
    .map(
        |((), instruction, comment, (), ())| match (instruction, comment) {
            (instruction, Some(comment)) => {
                Line::CodeAndComment(Statement::new(None, instruction), comment)
            }
            (instruction, None) => Line::Code(Statement::new(None, instruction)),
        },
    )
    .boxed();

    let section_label_comment_comment_instruction_comment =
        group((
            ws0,
            labelmarker,
            comment.then_ignore(nl).optional(),
            ws0.then(nl).optional(),
            comment
                .then_ignore(nl)
                .or(ws0.ignore_then(nl).slice())
                .repeated()
                .slice()
                .optional(),
            ws0,
            instruction
                .if_no_progress(ErrorMessage::ExpectedOtherToken {
                    expected: vec!["instruction".to_string()],
                })
                .cut(),
            comment.optional(),
            ws0,
            nl_or_eof.labelled("end of line").cut(),
        ))
        .map(
            |((), label, label_comment, _, commentlist, (), instruction, inst_comment, (), ())| {
                match (label, label_comment, commentlist, instruction, inst_comment) {
                    (label, label_comment, Some(commentlist), instruction, Some(inst_comment)) => {
                        Line::CodeAndComment(
                            Statement::new(
                                Some(((label, label_comment), Some(commentlist))),
                                instruction,
                            ),
                            inst_comment,
                        )
                    }
                    (label, label_comment, Some(commentlist), instruction, None) => {
                        Line::Code(Statement::new(
                            Some(((label, label_comment), Some(commentlist))),
                            instruction,
                        ))
                    }
                    (label, label_comment, None, instruction, Some(inst_comment)) => {
                        Line::CodeAndComment(
                            Statement::new(Some(((label, label_comment), None)), instruction),
                            inst_comment,
                        )
                    }
                    (label, label_comment, None, instruction, None) => Line::Code(Statement::new(
                        Some(((label, label_comment), None)),
                        instruction,
                    )),
                }
            },
        )
        .boxed();

    let section = choice((
        section_empty,
        section_comment,
        section_label_comment_comment_instruction_comment,
        section_instruction_maybecomment,
    ));
    let program = section
        .repeated()
        .map(|sections| Program { inner: sections })
        .then_ignore(ws0);
    program.parse_to_end(input)
}

// reg type, used similarly to a primitive
// val must be 0 <= val <= 7
#[derive(Clone, Copy)]
pub struct Reg {
    inner: u8,
    pub span: Span,
}

impl Reg {
    pub fn value(&self) -> u8 {
        self.inner
    }
}

impl std::fmt::Debug for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.inner)
    }
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "r{}", self.inner)
    }
}

#[derive(Debug, Clone)]
pub struct InstructionWithSpan<'source> {
    pub inst: Instruction<'source>,
    pub span: Span,
}

impl std::fmt::Display for InstructionWithSpan<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inst)
    }
}

impl<'source> InstructionWithSpan<'source> {
    pub fn new(inst: Instruction<'source>, span: Span) -> Self {
        Self { inst, span }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Label<'source>(pub &'source str, pub Span);

impl std::fmt::Display for Label<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LoadFrom<'source> {
    ImmediateNumber(Num),
    ImmediateLabel(Label<'source>),
    // offset must be a multiple of 4, and must be 0 <= offset <= 60
    // as it is represented as a hexit
    Offset { offset: Option<Num>, base: Reg },
    Indexed { base: Reg, index: Reg },
}

impl std::fmt::Display for LoadFrom<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadFrom::ImmediateNumber(n) => write!(f, "${n}"),
            LoadFrom::ImmediateLabel(l) => write!(f, "${l}"),
            LoadFrom::Offset { offset, base } => {
                if let Some(offset) = offset {
                    return write!(f, "{offset}({base})");
                }
                write!(f, "({base})")
            }
            LoadFrom::Indexed { base, index } => write!(f, "({base}, {index}, 4)"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum StoreTo {
    // offset must be a multiple of 4, and must be 0 <= offset <= 60
    // as it is represented as a hexit
    Offset { offset: Option<Num>, base: Reg },
    Indexed { base: Reg, index: Reg },
}

impl std::fmt::Display for StoreTo {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StoreTo::Offset { offset, base } => {
                if let Some(offset) = offset {
                    return write!(f, "{offset}({base})");
                }
                write!(f, "({base})")
            }
            StoreTo::Indexed { base, index } => {
                write!(f, "({base}, {index}, 4)")
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BranchLocation<'source> {
    Address(Num),
    Label(Label<'source>),
}

impl std::fmt::Display for BranchLocation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BranchLocation::Address(a) => write!(f, "{a}"),
            BranchLocation::Label(l) => write!(f, "{l}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DirectiveLongValue<'source> {
    Number(Num),
    Label(Label<'source>),
}

impl std::fmt::Display for DirectiveLongValue<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DirectiveLongValue::Number(n) => write!(f, "{n}"),
            DirectiveLongValue::Label(l) => write!(f, "{l}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum JumpLocation<'source> {
    Label(Label<'source>),
    // Must NOT be negative
    Addr(Num),
    // offset must be divisible by 2 and 0 <= 510 as it is stored in a single byte * 2
    Indirect { offset: Option<Num>, to: Reg },
    DoubleIndirect(DoubleIndirectMethod),
}

impl std::fmt::Display for JumpLocation<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JumpLocation::Label(l) => write!(f, "{l}"),
            JumpLocation::Addr(a) => write!(f, "{a}"),
            JumpLocation::Indirect { offset, to } => {
                if let Some(offset) = offset {
                    return write!(f, "{offset}({to})");
                }
                write!(f, "({to})")
            }
            JumpLocation::DoubleIndirect(di) => {
                write!(f, "{di}")
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DoubleIndirectMethod {
    // offset must be divisible by 4 and 0 <= 1020 as it is stored in a single byte * 4
    Offset { offset: Option<Num>, to: Reg },
    Indexed { base: Reg, index: Reg },
}

impl std::fmt::Display for DoubleIndirectMethod {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DoubleIndirectMethod::Offset { offset, to } => {
                if let Some(offset) = offset {
                    return write!(f, "*{offset}({to})");
                }
                write!(f, "({to})")
            }
            DoubleIndirectMethod::Indexed { base, index } => write!(f, "*({base}, {index}, 4)"),
        }
    }
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

impl std::fmt::Display for Instruction<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Load {
                from: (from, _),
                to,
            } => {
                write!(f, "ld {from}, {to}")
            }
            Instruction::Store { from, to: (to, _) } => write!(f, "st {from}, {to}"),
            Instruction::Halt => write!(f, "halt"),
            Instruction::Nop => write!(f, "nop"),
            Instruction::Mov { from, to } => write!(f, "mov {from}, {to}"),
            Instruction::Add { from, to } => write!(f, "add {from}, {to}"),
            Instruction::And { from, to } => write!(f, "and {from}, {to}"),
            Instruction::Inc { reg } => write!(f, "inc {reg}"),
            Instruction::IncAddr { reg } => write!(f, "inca {reg}"),
            Instruction::Dec { reg } => write!(f, "dec {reg}"),
            Instruction::DecAddr { reg } => write!(f, "deca {reg}"),
            Instruction::Not { reg } => write!(f, "not {reg}"),
            Instruction::ShiftLeft { amt, reg } => write!(f, "shl ${amt}, {reg}"),
            Instruction::ShiftRight { amt, reg } => write!(f, "shr ${amt}, {reg}"),
            Instruction::Branch { to: (to, _) } => write!(f, "br {to}"),
            Instruction::BranchIfEqual { reg, to: (to, _) } => write!(f, "beq {reg}, {to}"),
            Instruction::BranchIfGreater { reg, to: (to, _) } => write!(f, "bgt {reg}, {to}"),
            Instruction::GetProgramCounter { offset, to } => write!(f, "gpc ${offset}, {to}"),
            Instruction::Jump { to: (to, _) } => write!(f, "j {to}"),
            Instruction::Syscall { typ: (typ, _) } => write!(f, "sys ${typ}"),
            Instruction::DirectiveLong { value: (val, _) } => write!(f, ".long {val}"),
            Instruction::DirectivePos { loc: (loc, _) } => write!(f, ".pos {loc}"),
        }
    }
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
}

impl std::fmt::Display for SyscallType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyscallType::Read => write!(f, "0"),
            SyscallType::Write => write!(f, "1"),
            SyscallType::Exec => write!(f, "2"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Program<'source> {
    pub inner: Vec<Line<'source>>,
}

const TAB: &str = "    ";

impl std::fmt::Display for Program<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, line) in self.inner.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }
            write!(f, "{line}")?;
        }
        Ok(())
    }
}

type LabelComment<'source> = (Label<'source>, Option<&'source str>);

#[derive(Debug, Clone)]
pub struct Statement<'source> {
    // This exists so we can format and keep comment locations.
    pub label_and_comment: Option<(LabelComment<'source>, Option<&'source str>)>,
    pub instruction: InstructionWithSpan<'source>,
}

impl std::fmt::Display for Statement<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(((label, labelcomment), comments)) = self.label_and_comment {
            write!(f, "{label}:")?;
            if let Some(comment) = labelcomment {
                write!(f, "{comment}")?;
            }
            if let Some(comments) = comments {
                write!(f, "{comments}")?;
            }
            writeln!(f)?;
        }
        if let InstructionWithSpan {
            inst: Instruction::DirectivePos { .. },
            ..
        } = self.instruction
        {
            // .pos usually shouldn't be indented
            write!(f, "{}", self.instruction)
        } else {
            write!(f, "{TAB}{}", self.instruction)
        }
    }
}

impl<'source> Statement<'source> {
    pub const fn new(
        label: Option<(LabelComment<'source>, Option<&'source str>)>,
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
    Empty,
}

impl std::fmt::Display for Line<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Line::Code(code) => write!(f, "{code}"),
            Line::Comment(comment) => write!(f, "{comment}"),
            Line::CodeAndComment(code, comment) => {
                write!(f, "{code}{comment}")
            }
            Line::Empty => Ok(()),
        }
    }
}

impl<'source> Line<'source> {
    fn code(&self) -> Option<&Statement> {
        match self {
            Line::Code(c) => Some(c),
            Line::Comment(_) => None,
            Line::CodeAndComment(c, _) => Some(c),
            Line::Empty => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Span,
    pub related: Option<(String, Span)>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Severity {
    Error,
    Warning,
}

static KEYWORDS: &[&str] = &[
    "ld", "st", "halt", "nop", "mov", "add", "and", "inc", "inca", "dec", "deca", "not", "shl",
    "shr", "br", "beq", "bgt", "j", "gpc", "sys", "pos", "long",
];

#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct SecondPassOutput<'a, 'input> {
    pub inner: &'a Program<'input>,
}

pub fn second_pass<'a, 'input>(
    program: &'a Program<'input>,
) -> (Vec<Diagnostic>, Option<SecondPassOutput<'a, 'input>>) {
    let mut output = Vec::new();

    let mut labels: HashMap<&str, Label<'_>> = HashMap::new();

    for line in &program.inner {
        if let Some(s) = line.code() {
            if let Some(((l, _), _)) = s.label_and_comment {
                if let Some(map_l) = labels.get(l.0) {
                    output.push(Diagnostic {
                        severity: Severity::Error,
                        message: format!("Label `{}` declared previously", l.0),
                        span: l.1,
                        related: Some(("Label declared here".to_string(), map_l.1)),
                    });
                    continue;
                }
                labels.insert(l.0, l);
            }
        }
    }
    for (labelname, Label(_, span)) in &labels {
        if KEYWORDS.contains(labelname) {
            output.push(Diagnostic {
                severity: Severity::Warning,
                message: format!("Keyword used as label: `{labelname}`"),
                span: *span,
                related: None,
            })
        }
    }

    for line in &program.inner {
        if let Some(s) = line.code() {
            let InstructionWithSpan { ref inst, .. } = s.instruction;
            match inst {
                Instruction::Load {
                    from: (LoadFrom::ImmediateLabel(label), _),
                    ..
                }
                | Instruction::Branch {
                    to: (BranchLocation::Label(label), _),
                }
                | Instruction::BranchIfGreater {
                    to: (BranchLocation::Label(label), _),
                    ..
                }
                | Instruction::Jump {
                    to: (JumpLocation::Label(label), _),
                }
                | Instruction::BranchIfEqual {
                    to: (BranchLocation::Label(label), _),
                    ..
                } => {
                    if !labels.contains_key(label.0) {
                        output.push(Diagnostic {
                            severity: Severity::Error,
                            message: format!("Use of non-existent label `{}`", label.0),
                            span: label.1,
                            related: None,
                        })
                    }
                }
                _ => {}
            }
        }
    }

    /*
    TODO:
    once "assembler" is done, make a pass to verify that .pos points to a valid location.



    */

    if output
        .iter()
        .find(|err| err.severity == Severity::Error)
        .is_some()
    {
        (output, None)
    } else {
        (output, Some(SecondPassOutput { inner: program }))
    }
}

// fn format(p: &Program) {

// }

#![warn(clippy::nursery, clippy::pedantic, clippy::all)]
#![allow(clippy::similar_names)]

// use chumsky::prelude::*;
use parser::prelude::*;
use sm213_parser::*;

// macro_rules! assert_result_type_eq {
//     ($parser:ident, $ty:ty) => {{
//         _ = || {
//             let __assert_type: $ty = $parser.parse_to_end("").unwrap();
//         }
//     }};
// }

fn mult<'input>() -> impl Parser<'input> {
    int(36)
        .try_map_with_span(|s, span| {
            if s != "4" {
                return Err(ParseError {
                    message: ErrorMessage::Custom(format!("Multiplier must be 4, but found {s}")),
                    span_or_pos: SpanOrPos::Span(span),
                    kind: ParseErrorType::Backtrack,
                });
            }
            Ok(())
        })
        .ignored()
}

fn deci<'input>() -> impl Parser<'input, Output = Num> {
    "-".optional()
        .then(int(36).cut())
        .slice()
        .try_map_with_span(|s, span| {
            dbg!(&s);
            #[allow(clippy::cast_possible_wrap)]
            s.parse::<u32>()
                .map_err(|_e| ParseError {
                    message: ErrorMessage::Custom(format!("Couldn't parse decimal integer: `{s}`")),
                    span_or_pos: SpanOrPos::Span(span),
                    kind: ParseErrorType::Cut,
                })
                .map(|n| Num {
                    kind: NumType::Decimal,
                    value: n as i32,
                    span,
                })
        })
}

fn hex<'input>() -> impl Parser<'input, Output = Num> {
    "0x".labelled("hexadecimal number")
        // be lenient for better errors
        .ignore_then(int(36).cut())
        .try_map_with_span(|s, span| {
            #[allow(clippy::cast_possible_wrap)]
            u32::from_str_radix(s, 16)
                .map_err(|_e| ParseError {
                    message: ErrorMessage::Custom(format!(
                        "Couldn't parse hexadecimal integer: `{s}`"
                    )),
                    span_or_pos: SpanOrPos::Span(span),
                    kind: ParseErrorType::Cut,
                })
                .map(|n| Num {
                    kind: NumType::Hexadecimal,
                    value: n as i32,
                    span,
                })
        })
}

fn reg<'input>() -> impl Parser<'input, Output = Reg> {
    "r".labelled("register")
        // parse all alphanumerics for better errors
        .ignore_then(
            ('0'..='7')
                .to_char_range()
                .labelled("register number (0-7)")
                .cut(),
        )
        .map_with_span(|s, span| Reg {
            inner: s.parse().unwrap(),
            span,
        })
}

fn ws0<'input>() -> impl Parser<'input, Output = ()> {
    " ".or("\t").repeated().ignored()
}

fn ws1<'input>() -> impl Parser<'input, Output = ()> {
    " ".or("\t")
        .repeated()
        .at_least(1)
        .labelled("whitespace")
        .ignored()
}

fn nl<'input>() -> impl Parser<'input, Output = ()> {
    "\r\n".or("\r").or("\n").labelled("newline").ignored()
}

fn nl_or_eof<'input>() -> impl Parser<'input, Output = ()> {
    nl().or(EndOfInput)
}
fn comma<'input>() -> impl Parser<'input, Output = ()> {
    ",".padded_by(ws0()).ignored()
}
fn open_paren<'input>() -> impl Parser<'input, Output = ()> {
    "(".padded_by(ws0()).ignored()
}
fn close_paren<'input>() -> impl Parser<'input, Output = ()> {
    ")".padded_by(ws0()).ignored()
}

fn integer<'input>() -> impl Parser<'input, Output = Num> {
    hex().or(deci()).labelled("hex or dec integer")
}

fn label<'input>() -> impl Parser<'input, Output = Label<'input>> {
    (Alpha.or("_").labelled("label"))
        .then(AlphaNumeric.or("_").repeated())
        .slice()
        .map_with_span(Label)
}

fn labelmarker<'input>() -> impl Parser<'input, Output = Label<'input>> {
    label.then_ignore(ws0.then(":"))
}

fn immediate_num<'input>() -> impl Parser<'input, Output = Num> {
    ("$".then(ws0)).ignore_then(integer)
}

fn comment<'input>() -> impl Parser<'input, Output = &'input str> {
    ws0.then("#")
        .then(Any1.and_is(nl_or_eof.not()).repeated())
        .slice()
}

fn base_plus_offset<'input>() -> impl Parser<'input, Output = (Option<Num>, Reg)> {
    integer
        .optional()
        .then_ignore(ws0)
        .then(reg.padded_by(ws0).delimited_by("(", ")"))
}

fn indexed<'input>() -> impl Parser<'input, Output = (Reg, Reg)> {
    open_paren()
        .ignore_then(reg.padded_by(ws0))
        .then_ignore(comma())
        .then(reg.padded_by(ws0))
        .then_ignore(comma())
        .then_ignore(mult().padded_by(ws0))
        .then_ignore(close_paren())
}

fn ld<'input>() -> impl Parser<'input, Output = InstructionWithSpan<'input>> {
    let ld_base_plus_offset = base_plus_offset.try_map_with_span(|(offset, base), span| {
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
    let ld_indexed =
        indexed.map_with_span(|(base, index), span| (LoadFrom::Indexed { base, index }, span));

    "ld".ignore_then(ws1)
        .ignore_then(
            (("$".then(ws0)).ignore_then(
                label
                    .map_with_span(|label, span| (LoadFrom::ImmediateLabel(label), span))
                    .or(integer.map_with_span(|int, span| (LoadFrom::ImmediateNumber(int), span)))
                    .if_no_progress("expected label or integer"),
            ))
            .or(ld_base_plus_offset)
            .or(ld_indexed)
            .if_no_progress(
                "Expected immediate number, immediate label, offset access, or indexed access",
            )
            .cut(),
        )
        .then_ignore(comma())
        .then(reg)
        .map_with_span(|(from, reg), span| {
            InstructionWithSpan::new(Instruction::Load { from, to: reg }, span)
        })
}

fn st<'input>() -> impl Parser<'input, Output = InstructionWithSpan<'input>> {
    let st_base_plus_offset = base_plus_offset.try_map_with_span(|(offset, base), span| {
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
    let st_indexed =
        indexed.map_with_span(|(base, index), span| (StoreTo::Indexed { base, index }, span));
    "st".ignore_then(ws1)
        .ignore_then(reg)
        .then_ignore(comma())
        .then(
            st_base_plus_offset
                .or(st_indexed)
                .if_no_progress("Expected offset access or indexed access")
                .cut(), /* .or(expected("base plus offset, or indexed")),*/
        )
        .map_with_span(|(reg, to), span| {
            InstructionWithSpan::new(Instruction::Store { from: reg, to }, span)
        })
}

fn jmp_location<'input>() -> impl Parser<'input, Output = (JumpLocation<'input>, Span)> {
    choice((
        base_plus_offset.try_map_with_span(|(offset, base), span| {
            if let Some(offset) = offset {
                if !((0..=508).contains(&offset) && offset % 2 == 0) {
                    return Err(ParseError {
                        message: ErrorMessage::Custom(format!(
                            "gpc offset should be 0 <= offset <= 508 and divisible by 2, found `{offset}`"
                        )),
                        span_or_pos: SpanOrPos::Span(span),
                        kind: ParseErrorType::Cut,
                    });
                }
                }
            Ok((JumpLocation::Indirect { offset, to: base }, span))
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
        label.try_map_with_span(|to, span| {
            Ok((JumpLocation::Label(to), span))
        }),
        "*".ignore_then(
            base_plus_offset.try_map_with_span(|(offset, base), span| {
                if let Some(offset) = offset {
                    if !((0..=1020).contains(&offset) && offset % 4 == 0) {
                        return Err(ParseError {
                            message: ErrorMessage::Custom(format!(
                                "gpc offset should be 0 <= offset <= 1020 and divisible by 4, found `{offset}`"
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
    ))
}

#[allow(clippy::too_many_lines)]
fn main() {
    let halt = "halt"
        .to_span()
        .map(|sp| InstructionWithSpan::new(Instruction::Halt, sp));
    let nop = "nop"
        .to_span()
        .map(|sp| InstructionWithSpan::new(Instruction::Nop, sp));
    let mov = "mov"
        .then(ws1)
        .ignore_then(reg.cut())
        .then_ignore(comma())
        .then(reg.cut())
        .map_with_span(|(from, to), span| {
            InstructionWithSpan::new(Instruction::Mov { from, to }, span)
        });
    let add = "add"
        .then(ws1)
        .ignore_then(reg.cut())
        .then_ignore(comma())
        .then(reg.cut())
        .map_with_span(|(from, to), span| {
            InstructionWithSpan::new(Instruction::Add { from, to }, span)
        });
    let and = "and"
        .then(ws1)
        .ignore_then(reg.cut())
        .then_ignore(comma())
        .then(reg.cut())
        .map_with_span(|(from, to), span| {
            InstructionWithSpan::new(Instruction::And { from, to }, span)
        });
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
        .then_ignore(comma())
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
        });
    let shr = "shr"
        .then(ws1)
        .ignore_then(immediate_num.cut())
        .then_ignore(comma())
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
        });
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
        Ok((BranchLocation::Address(to), span))
    });
    let br = "br"
        .ignore_then(ws1)
        .ignore_then(br_label.or(br_addr).or(expected("label or address")).cut())
        .map_with_span(|to, span| InstructionWithSpan::new(Instruction::Branch { to }, span));
    let beq = "beq"
        .ignore_then(ws1)
        .ignore_then(reg)
        .then_ignore(comma())
        .then(br_label.or(br_addr).or(expected("label or address")).cut())
        .map_with_span(|(reg, to), span| {
            InstructionWithSpan::new(Instruction::BranchIfEqual { reg, to }, span)
        });
    // let bgt = bgt_label.or(bgt_addr);
    let bgt = "bgt"
        .ignore_then(ws1)
        .ignore_then(reg)
        .then_ignore(comma())
        .then(br_label.or(br_addr).or(expected("label or address")).cut())
        .map_with_span(|(reg, to), span| {
            InstructionWithSpan::new(Instruction::BranchIfGreater { reg, to }, span)
        });

    let jmp = "j"
        .ignore_then(ws1)
        .ignore_then(jmp_location)
        .map_with_span(|loc, span| InstructionWithSpan::new(Instruction::Jump { to: loc }, span));
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
        });
    let gpc = "gpc"
        .then(ws1)
        .ignore_then(immediate_num.cut())
        .then_ignore(comma())
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
        });
    let directive = "."
        .ignore_then(
            "long"
                .ignore_then(ws1)
                .ignore_then(
                    label
                        .map_with_span(|l, span| (DirectiveLongValue::Label(l), span))
                        .or(integer
                            .map_with_span(|num, span| (DirectiveLongValue::Number(num), span)))
                        .if_no_progress("Expected label or number")
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
                .if_no_progress("Expected `long` or `pos`")
                .cut(),
        )
        .map_with_span(InstructionWithSpan::new);

    let instruction = choice((
        ld, st, halt, nop, mov, add, and, inc, inca, dec, deca, not, shl, shr, br, beq, bgt, jmp,
        sys, directive, gpc,
    ));

    let section_empty = ws0.then_ignore(nl()).to(Line::Empty);
    let section_comment = comment.then_ignore(nl_or_eof).map(Line::Comment);
    let section_instruction_maybecomment = ws0
        .ignore_then(instruction)
        .then(comment.optional())
        .then_ignore(ws0)
        .then_ignore(nl_or_eof)
        .map(|(instruction, comment)| match (instruction, comment) {
            (instruction, Some(comment)) => {
                Line::CodeAndComment(Statement::new(None, instruction), comment)
            }
            (instruction, None) => Line::Code(Statement::new(None, instruction)),
        });
    let section_label_comment_comment_instruction_comment = group((
        ws0,
        labelmarker,
        comment.then_ignore(nl).optional(),
        comment
            .then_ignore(nl)
            .or(ws0.ignore_then(nl).slice())
            .repeated()
            .slice()
            .optional(),
        instruction,
        comment.optional(),
        nl_or_eof,
    ));
    /*
    labelmarker: #comment
    #comment
    \n\n
    # comment
    \n
    instruction #comment \n_or_eof

    */
}

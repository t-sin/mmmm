use nom::branch::alt;
use nom::combinator::{all_consuming, map, opt, rest_len, value};
use nom::multi::{many0, separated_list};
use nom::sequence::{delimited, preceded, tuple};
use nom::{Err, IResult};

use crate::tokenize::{token_type_eq, Keyword, Operator, Special, Token};

/// Represents error types while parsing.
#[derive(Debug)]
pub enum ErrorKind {
    Nom(nom::error::ErrorKind),
    ExpressionStackIsEmpty,
    UnexpectedToken(Token),
    UnexpectedEof,
    CannotParseExpression,
    InvalidInvokeFn,
}

/// Represents parser errors.
///
/// Note: it's not a nom::error::ParseError.
#[derive(Debug)]
pub struct ParseError<I> {
    input: I,
    kind: ErrorKind,
}

impl<I> ParseError<I> {
    /// creates ParseError from ErrorKind
    fn new(input: I, kind: ErrorKind) -> Self {
        ParseError {
            input: input,
            kind: kind,
        }
    }
}

impl<I> nom::error::ParseError<I> for ParseError<I> {
    /// converts nom::error::ErrorKind to mmmm's ParseError
    fn from_error_kind(input: I, kind: nom::error::ErrorKind) -> Self {
        ParseError {
            input: input,
            kind: ErrorKind::Nom(kind),
        }
    }

    fn append(_: I, _: nom::error::ErrorKind, other: Self) -> Self {
        other
    }
}

/// a type of parser's input
type Input<'a> = &'a [Token];

/// a return value type of combinators defined this file
type CombinatorResult<'a> = IResult<Input<'a>, &'a Token, ParseError<Input<'a>>>;
/// a return value type of expression parts parser
type ParseExp1Result<'a> = Result<(), Err<ParseError<Input<'a>>>>;
/// a return value type of expression parser
type ParseExpResult<'a> = IResult<Input<'a>, Exp, ParseError<Input<'a>>>;
/// a retrun value type of statement parser
type ParseStmtResult<'a> = IResult<Input<'a>, Option<Statement>, ParseError<Input<'a>>>;
/// a return value type of parser
type ParseResult<'a> = IResult<Input<'a>, Option<AST>, ParseError<Input<'a>>>;

/// Represents mmmm's operator associativity.
enum OperatorAssociativity {
    Left,
    Right,
    None,
}

/// Recognize one token.
fn token<'a>(token: Token) -> impl Fn(&'a [Token]) -> CombinatorResult<'a> {
    move |t| {
        let (t, len) = rest_len(t)?;
        if len > 0 {
            if &t[0] == &token {
                Ok((&t[1..], &t[0]))
            } else {
                Err(Err::Error(ParseError::new(
                    &t[..],
                    ErrorKind::UnexpectedToken(t[0].clone()),
                )))
            }
        } else {
            Err(Err::Error(ParseError::new(
                &t[..],
                ErrorKind::UnexpectedEof,
            )))
        }
    }
}

/// Recognize one token by checking token type.
///
/// It means this combinator ignores enum variants' union value.
fn token_type_of<'a>(token: Token) -> impl Fn(&'a [Token]) -> CombinatorResult<'a> {
    move |t| {
        let (t, len) = rest_len(t)?;
        if len > 0 {
            if token_type_eq(&t[0], &token) {
                Ok((&t[1..], &t[0]))
            } else {
                Err(Err::Error(ParseError::new(
                    &t[..],
                    ErrorKind::UnexpectedToken(t[0].clone()),
                )))
            }
        } else {
            Err(Err::Error(ParseError::new(
                &t[..],
                ErrorKind::UnexpectedEof,
            )))
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Represents mmmm's identifier.
pub struct Symbol(pub String);

#[derive(Debug, PartialEq, Clone)]
/// Represents mmmm's function invokation.
pub struct InvokeFn(pub Box<Symbol>, pub Vec<Exp>);

#[derive(Debug, PartialEq, Clone)]
/// Represents mmmm's if expression.
pub struct If {
    pub cond: Box<Exp>,
    pub true_clause: Vec<Exp>,
    pub false_clause: Option<Vec<Exp>>,
}

/// Represents variable declaration with type.
///
/// The type may be None when omitted by user.
#[derive(Debug, PartialEq, Clone)]
pub enum Declare {
    Var(Box<Symbol>, Option<Keyword>),
}

/// Represents mmmm's function objects.
#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    pub args: Vec<Declare>,
    pub ret_type: Option<Keyword>,
    pub body: Block,
}

#[derive(Debug, PartialEq, Clone)]
/// Represents mmmm's expressions.
pub enum Exp {
    Float(f64),
    String(String),
    Special(Box<Special>),
    Variable(Box<Symbol>),
    Fn(Box<Function>),
    InvokeFn(Box<InvokeFn>),
    UnaryOp(Box<Operator>, Box<Exp>),
    BinaryOp(Box<Operator>, Box<Exp>, Box<Exp>),
    PostOp(Box<Operator>, Box<Symbol>, Box<Exp>),
    If(Box<If>),
}

#[derive(Debug, PartialEq, Clone)]
/// Represents statements; semantic parts of program.
pub enum Statement {
    Exp(Box<Exp>),
    Assign(Box<Symbol>, Box<Exp>),
    Return(Box<Exp>),
    InvokeAt(Box<InvokeFn>, Box<Exp>),
}

/// Represents block.
pub type Block = Vec<Statement>;

#[derive(Debug, PartialEq, Clone)]
/// Represents mmmm's abstruct syntax tree.
///
/// Note: Now can parse only expressions.
pub enum AST {
    Statement(Box<Statement>),
    Block(Block),
    Defun(Box<Symbol>, Box<Function>),
}

/// Gives associativity to the specified operator name.
fn operator_associativity(op: Operator) -> OperatorAssociativity {
    match op {
        Operator::Member => OperatorAssociativity::Left,
        Operator::Access => OperatorAssociativity::None,
        Operator::Not => OperatorAssociativity::Right,
        Operator::Plus
        | Operator::Minus
        | Operator::Multiply
        | Operator::Divide
        | Operator::Mod
        | Operator::Power => OperatorAssociativity::Left,
        Operator::Gte | Operator::Lte | Operator::Eq => OperatorAssociativity::Left,
        Operator::Gt | Operator::Lt => OperatorAssociativity::Left,
        Operator::Or | Operator::And => OperatorAssociativity::Left,
    }
}

/// Gives a number of operator precedence to the specified operator name.
///
/// Lesser operator precedence number means the operators has greater precedence.
fn operator_precedence(op: Operator) -> i32 {
    match op {
        Operator::Access => 1,
        Operator::Member => 1, // メンバアクセス演算子。まだない。
        Operator::Not => 2,    // ないけど
        Operator::Power => 3,
        Operator::Multiply | Operator::Divide | Operator::Mod => 4,
        Operator::Plus | Operator::Minus => 5,
        Operator::Gte | Operator::Lte | Operator::Eq => 6,
        Operator::Gt | Operator::Lt => 6,
        Operator::And => 7,
        Operator::Or => 8,
    }
}

#[derive(Debug)]
/// Structures for the state of expression parsing.
///
/// This structure's state is a state for modified shunting yard algorithm.
struct ParseExpState<'a> {
    /// A number of nesting of parse_exp_1. It's for debugging.
    nest: u32,
    /// A rest of inputs
    input: &'a [Token],
    /// An output queue.
    output: Vec<Exp>,
    /// An operator stack.
    stack: Vec<Token>,
    /// A token for the previous parse_exp_1.
    prev_token: Option<Token>,
}

/// Terminates shunting-yard algorithm with ParseExpState.
///
/// In this phase, it pops an operator token from the operator stack in state, the operator have low precedence
/// in `parse_exp_1_op`, then it pops their operands from the output queue and make Exp::BinaryOp.
/// When the output queue is empty while making Exp::BinaryOp, rise error ErrorKind::ExpressionStackIsEmpty.
fn terminate_parse_exp_1<'a>(state: &mut ParseExpState<'a>) -> ParseExp1Result<'a> {
    loop {
        match state.stack.pop() {
            Some(Token::Op(op)) => {
                if let (Some(exp2), Some(exp1)) = (state.output.pop(), state.output.pop()) {
                    let exp = Exp::BinaryOp(op.clone(), Box::new(exp1), Box::new(exp2));
                    state.output.push(exp);
                } else {
                    return Err(Err::Error(ParseError::new(
                        &state.input[..],
                        ErrorKind::ExpressionStackIsEmpty,
                    )));
                }
            }
            None => break,
            _ => {
                return Err(Err::Error(ParseError::new(
                    &state.input[..],
                    ErrorKind::ExpressionStackIsEmpty,
                )))
            }
        }
    }

    Ok(())
}

/// Parses function invokation.
fn parse_invoke_fn<'a>(t: &'a [Token]) -> IResult<Input<'a>, InvokeFn, ParseError<Input<'a>>> {
    match tuple((
        token_type_of(Token::Identifier("".to_string())),
        delimited(
            token(Token::OpenParen),
            separated_list(token(Token::Comma), parse_expression),
            token(Token::CloseParen),
        ),
    ))(t)
    {
        Ok((rest, (name, args))) => {
            if let Token::Identifier(name) = name {
                Ok((rest, InvokeFn(Box::new(Symbol(name.clone())), args)))
            } else {
                Err(Err::Error(ParseError {
                    input: t,
                    kind: ErrorKind::InvalidInvokeFn,
                }))
            }
        }
        _ => Err(Err::Error(ParseError {
            input: t,
            kind: ErrorKind::InvalidInvokeFn,
        })),
    }
}

/// Parses identifiers related expressions.
///
/// This function parses three kind of expressions:
///
///  - function invokation
///  - array accessing
///  - variable accessing
///
/// Three kind of expressions above are proceeded by an identifier, so this function distinguishes by
/// checking next token in ParseExpState.
fn parse_exp_1_identifier<'a>(name: &String, state: &mut ParseExpState<'a>) -> ParseExp1Result<'a> {
    match state.input.iter().nth(1) {
        Some(Token::OpenParen) => {
            // function invokation
            match parse_invoke_fn(state.input) {
                Ok((rest, invoke_fn)) => {
                    state.input = rest;
                    state.prev_token = Some(Token::CloseParen);
                    let exp = Exp::InvokeFn(Box::new(invoke_fn));
                    state.output.push(exp);
                }
                Err(_) => {
                    return Err(Err::Error(ParseError {
                        input: state.input,
                        kind: ErrorKind::InvalidInvokeFn,
                    }))
                }
            };
        }
        Some(Token::OpenBracket) => {
            // array access
            match tuple((
                token_type_of(Token::Identifier("".to_string())),
                delimited(
                    token(Token::OpenBracket),
                    parse_expression,
                    token(Token::CloseBracket),
                ),
            ))(state.input)
            {
                Ok((rest, (_, exp))) => {
                    state.input = rest;
                    state.prev_token = Some(Token::CloseBracket);
                    let exp = Exp::PostOp(
                        Box::new(Operator::Access),
                        Box::new(Symbol(name.to_string())),
                        Box::new(exp),
                    );
                    state.output.push(exp);
                }
                Err(err) => return Err(err),
            }
        }
        _ => {
            state.input = &state.input[1..];
            // variable
            let exp = Exp::Variable(Box::new(Symbol(name.to_string())));
            state.output.push(exp);
        }
    };

    Ok(())
}

/// Parses sub expressions delimited by '(' and ')'.
fn parse_exp_1_subexp<'a>(state: &mut ParseExpState<'a>) -> ParseExp1Result<'a> {
    match delimited(
        token(Token::OpenParen),
        parse_expression,
        token(Token::CloseParen),
    )(state.input)
    {
        Ok((rest, exp)) => {
            state.input = rest;
            state.prev_token = Some(Token::CloseParen);
            state.output.push(exp);
            Ok(())
        }
        Err(err) => Err(err),
    }
}

/// Distinguishes wheather the token `op` is unary operators.
///
/// For example, unary `-` token, to determine it is unary `-` it must be know previous token.
fn is_unary(op: Operator, prev_token: Option<Token>) -> bool {
    match op {
        Operator::Minus => match prev_token {
            Some(Token::Float(_)) => false,
            Some(Token::Op(op)) => match *op {
                Operator::Minus => false,
                _ => true,
            },
            Some(Token::OpenParen) => true,
            Some(_) => false,
            None => true,
        },
        _ => false,
    }
}

/// Parses unary/binary operators.
///
/// This algorithm is based on shunting-yard algorithm but few points are modified.
/// That point is immediately creating AST.
fn parse_exp_1_op<'a>(
    op1: &Operator,
    token: Option<Token>,
    state: &mut ParseExpState<'a>,
) -> ParseExp1Result<'a> {
    if is_unary(op1.clone(), state.prev_token.clone()) {
        if let Err(err) = parse_exp_1(state) {
            return Err(err);
        } else {
            if let Some(exp) = state.output.pop() {
                let exp = Exp::UnaryOp(Box::new(op1.clone()), Box::new(exp));
                state.output.push(exp);
            } else {
                return Err(Err::Error(ParseError::new(
                    &state.input[..],
                    ErrorKind::ExpressionStackIsEmpty,
                )));
            }
        }
    } else {
        loop {
            if let Some(Token::Op(op2)) = state.stack.last() {
                // if op2 has high precedence (lesser number) than op1, immediately pop op2 and construct Exp::BinaryOp
                // so op1 has lesser precedence number (it means low precedence), immediately breaks this loop
                match operator_associativity(op1.clone()) {
                    OperatorAssociativity::Left => {
                        if operator_precedence(op1.clone()) < operator_precedence(*op2.clone()) {
                            break;
                        }
                    }
                    OperatorAssociativity::Right => {
                        if operator_precedence(op1.clone()) <= operator_precedence(*op2.clone()) {
                            break;
                        }
                    }
                    OperatorAssociativity::None => {
                        panic!("oh {:?} is not an operator. it may be a bug!", op1);
                    }
                };

                if let (Some(Token::Op(op)), Some(exp2), Some(exp1)) =
                    (state.stack.pop(), state.output.pop(), state.output.pop())
                {
                    state
                        .output
                        .push(Exp::BinaryOp(op, Box::new(exp1), Box::new(exp2)));
                } else {
                    return Err(Err::Error(ParseError::new(
                        &state.input[..],
                        ErrorKind::ExpressionStackIsEmpty,
                    )));
                }
            } else {
                break;
            }
        }
        state.stack.push(token.unwrap().clone());
    }

    Ok(())
}

/// Parses an if expression.
fn parse_exp_1_if<'a>(state: &mut ParseExpState<'a>) -> ParseExp1Result<'a> {
    match preceded(
        token(Token::Keyword(Box::new(Keyword::If))),
        tuple((
            delimited(
                token(Token::OpenParen),
                parse_expression,
                token(Token::CloseParen),
            ),
            alt((
                delimited(
                    token(Token::OpenBrace),
                    many0(tuple((parse_expression, opt(token(Token::Newline))))),
                    token(Token::CloseBrace),
                ),
                map(parse_expression, |exp| vec![(exp, None)]),
            )),
            opt(preceded(
                token(Token::Keyword(Box::new(Keyword::Else))),
                alt((
                    delimited(
                        token(Token::OpenBrace),
                        many0(tuple((parse_expression, opt(token(Token::Newline))))),
                        token(Token::CloseBrace),
                    ),
                    map(parse_expression, |exp| vec![(exp, None)]),
                )),
            )),
        )),
    )(state.input)
    {
        Ok((rest, (cond, true_clause, false_clause))) => {
            state.input = rest;
            state.prev_token = Some(Token::CloseBrace);
            let true_clause = true_clause
                .iter()
                .map(|(e, _)| e.clone())
                .collect::<Vec<_>>();
            let false_clause = match false_clause {
                Some(expvec) => Some(expvec.iter().map(|(e, _)| e.clone()).collect::<Vec<_>>()),
                None => None,
            };
            let exp = Exp::If(Box::new(If {
                cond: Box::new(cond),
                true_clause: true_clause,
                false_clause: false_clause,
            }));
            state.output.push(exp);
            Ok(())
        }
        Err(err) => Err(err),
    }
}

/// Parses a lambda expressions.
fn parse_lambda_exp<'a>(state: &mut ParseExpState<'a>) -> ParseExp1Result<'a> {
    println!("{:?}", state);
    match tuple((
        delimited(token(Token::Bar), parse_function_arglist, token(Token::Bar)),
        parse_block,
    ))(state.input)
    {
        Ok((rest, (args, block))) => {
            let exp = Exp::Fn(Box::new(Function {
                args: args,
                ret_type: None,
                body: block,
            }));
            state.input = rest;
            state.prev_token = Some(Token::CloseBrace);
            state.output.push(exp);
            Ok(())
        }
        Err(err) => Err(err),
    }
}

/// Parses a part of expression.
///
/// Parsing expressions is a process that loops this function unless at the end of expressions (see `end_of_exp()`).
fn parse_exp_1<'a>(state: &mut ParseExpState<'a>) -> ParseExp1Result<'a> {
    let token = state.input.iter().nth(0);
    let input = state.input;
    let prev_input = state.input;
    if state.input.len() > 0 {
        state.input = &state.input[1..];
    }

    let result = match token {
        Some(Token::Float(f)) => {
            state.output.push(Exp::Float(*f));
            Ok(())
        }
        Some(Token::String(s)) => {
            state.output.push(Exp::String(s.to_string()));
            Ok(())
        }
        Some(Token::Keyword(kw)) => match **kw {
            Keyword::If => {
                state.input = prev_input;
                parse_exp_1_if(state)
            }
            _ => Err(Err::Error(ParseError::new(
                &state.input[..],
                ErrorKind::UnexpectedToken(token.unwrap().clone()),
            ))),
        },
        Some(Token::Special(sp)) => {
            state.output.push(Exp::Special(sp.clone()));
            Ok(())
        }
        Some(Token::Identifier(name)) => {
            state.input = prev_input;
            parse_exp_1_identifier(name, state)
        }
        Some(Token::Bar) => {
            state.input = prev_input;
            parse_lambda_exp(state)
        }
        Some(Token::OpenParen) => {
            state.input = input;
            parse_exp_1_subexp(state)
        }
        Some(Token::Op(op1)) => {
            let result = parse_exp_1_op(op1, Some(token.unwrap().clone()), state);
            state.prev_token = Some(token.unwrap().clone());
            result
        }
        Some(Token::LineComment(_))
        | Some(Token::OpenBrace)
        | Some(Token::OpenBracket)
        | Some(Token::FnReturnType)
        | Some(Token::Assign)
        | Some(Token::TimeAt) => Err(Err::Error(ParseError::new(
            &state.input[..],
            ErrorKind::UnexpectedToken(token.unwrap().clone()),
        ))),
        Some(Token::Comma) => Ok(()),
        Some(Token::Colon) => Err(Err::Error(ParseError::new(
            &state.input[1..],
            ErrorKind::UnexpectedToken(token.unwrap().clone()),
        ))),
        Some(Token::Newline)
        | Some(Token::CloseParen)
        | Some(Token::CloseBracket)
        | Some(Token::CloseBrace)
        | None => terminate_parse_exp_1(state),
    };

    state.prev_token = if let Some(token) = token {
        Some(token.clone())
    } else {
        None
    };

    result
}

/// Determines if the expression parsing is at the end of expression.
///
/// The expression parser meets end-of-expression things, e.g. closing delimiter,
/// the parser exits its process by this function.
fn end_of_exp(state: &mut ParseExpState) -> bool {
    if state.input.len() == 0 {
        return true;
    }

    match state.input.iter().nth(0).unwrap() {
        Token::Keyword(kw) => match **kw {
            Keyword::If => false,
            _ => true,
        },
        Token::Comma => true,
        Token::Newline => true,
        Token::CloseParen => true,
        Token::CloseBracket => true,
        Token::OpenBrace | Token::CloseBrace => true,
        _ => false,
    }
}

/// Parses an expression.
///
/// The purpose is parsing expression including infix notation by operator precedence,
/// so here uses shunting-yard algorithm.
/// This loops parsing towards end of expressions, then collecting rest operators and its operands,
/// then returns the result.
fn parse_expression(t: &[Token]) -> ParseExpResult {
    let mut state = ParseExpState {
        nest: 0,
        input: t,
        output: Vec::new(),
        stack: Vec::new(),
        prev_token: None,
    };

    while !end_of_exp(&mut state) {
        if let Err(err) = parse_exp_1(&mut state) {
            return Err(err);
        }
    }

    if let Err(err) = terminate_parse_exp_1(&mut state) {
        return Err(err);
    }

    if let Some(exp) = state.output.pop() {
        Ok((state.input, exp))
    } else {
        Err(Err::Error(ParseError::new(
            state.input,
            ErrorKind::CannotParseExpression,
        )))
    }
}

/// Parses an expressions as a statement.
///
/// The `parse_expression` returns Exp type because of recursive call in parsing expression.
/// But, entirely, the most parsing functions returns `Option<AST>` so this function wraps it.
fn parse_expression_stmt(t: &[Token]) -> ParseStmtResult {
    match parse_expression(t) {
        Ok((rest, exp)) => Ok((rest, Some(Statement::Exp(Box::new(exp))))),
        Err(err) => Err(err),
    }
}

/// Parses assinment statement.
fn parse_assignment(t: &[Token]) -> ParseStmtResult {
    match tuple((
        token_type_of(Token::Identifier("".to_string())),
        token_type_of(Token::Assign),
        parse_expression_stmt,
    ))(t)
    {
        Ok((rest, (Token::Identifier(name), _, ref stmt))) => {
            if let Some(Statement::Exp(exp)) = stmt {
                let stmt = Statement::Assign(Box::new(Symbol(name.to_string())), exp.clone());
                Ok((rest, Some(stmt)))
            } else {
                Err(Err::Error(ParseError::new(rest, ErrorKind::UnexpectedEof)))
            }
        }
        Ok((rest, (_, _, _))) => Err(Err::Error(ParseError::new(
            rest,
            ErrorKind::UnexpectedToken(Token::Newline),
        ))),
        Err(err) => Err(err),
    }
}

/// Parses return statement.
fn parse_return(t: &[Token]) -> ParseStmtResult {
    match tuple((
        token(Token::Keyword(Box::new(Keyword::Return))),
        parse_expression,
    ))(t)
    {
        Ok((rest, (_, exp))) => Ok((rest, Some(Statement::Return(Box::new(exp))))),
        Err(err) => Err(err),
    }
}

/// Parses scheduled function invokation.
fn parse_invoke_at(t: &[Token]) -> ParseStmtResult {
    match tuple((parse_invoke_fn, token(Token::TimeAt), parse_expression))(t) {
        Ok((rest, (invoke, _, exp))) => Ok((
            rest,
            Some(Statement::InvokeAt(Box::new(invoke), Box::new(exp))),
        )),
        Err(err) => Err(err),
    }
}

/// Parses a statement.
fn parse_statement(t: &[Token]) -> ParseResult {
    match alt((
        parse_expression_stmt,
        parse_assignment,
        parse_return,
        parse_invoke_at,
    ))(t)
    {
        Ok((rest, Some(stmt))) => Ok((rest, Some(AST::Statement(Box::new(stmt))))),
        Ok((rest, None)) => Ok((rest, None)),
        Err(err) => Err(err),
    }
}

/// Parses function arguments list in function definition.
///
/// The function argument types might be omitted by each arguments, so the the type specifier's
/// type is Option<Symbol>.
/// It is same behaviour with `parse_function_definitions`.
fn parse_function_arglist(t: &[Token]) -> IResult<Input, Vec<Declare>, ParseError<Input>> {
    match separated_list(
        token(Token::Comma),
        tuple((
            token_type_of(Token::Identifier("".to_string())),
            // function return type
            opt(tuple((
                token(Token::Colon),
                // type names
                alt((
                    token(Token::Keyword(Box::new(Keyword::Void))),
                    token(Token::Keyword(Box::new(Keyword::Float))),
                )),
            ))),
        )),
    )(t)
    {
        Ok((rest, args)) => {
            let args = args
                .into_iter()
                .map(|t| match t {
                    (Token::Identifier(name), Some((_, Token::Keyword(kw)))) => {
                        Declare::Var(Box::new(Symbol(name.to_string())), Some(*kw.clone()))
                    }
                    (Token::Identifier(name), None) => {
                        Declare::Var(Box::new(Symbol(name.to_string())), None)
                    }
                    _ => panic!("unreached here because of matching Token::Identifier"),
                })
                .collect();
            Ok((rest, args))
        }
        Err(err) => Err(err),
    }
}

/// Parses a block (list of statements).
///
/// Blocks appear like at function body, if clauses, or as an expression.
/// Empty lines are allowed.
fn parse_block(t: &[Token]) -> IResult<Input, Block, ParseError<Input>> {
    // parse function body
    match delimited(
        token(Token::OpenBrace),
        many0(alt((
            parse_expression_stmt,
            parse_assignment,
            parse_return,
            parse_invoke_at,
            value(None, token(Token::Newline)),
            value(None, token_type_of(Token::LineComment("".to_string()))),
        ))),
        token(Token::CloseBrace),
    )(t)
    {
        Ok((rest, stmt_vec)) => Ok((
            rest,
            stmt_vec
                .into_iter()
                .filter(|o| if let None = o { false } else { true })
                .map(|o| o.unwrap())
                .collect(),
        )),
        Err(err) => Err(err),
    }
}

/// Parses a function definition.
///
/// The function return type might be omitted, so the the type specifier's
/// type is Option<Symbol>.
/// It is same behaviour with `parse_function_arglist`.
fn parse_function_definition(t: &[Token]) -> ParseResult {
    match tuple((
        token(Token::Keyword(Box::new(Keyword::Fn))),
        many0(token(Token::Newline)),
        token_type_of(Token::Identifier("".to_string())),
        many0(token(Token::Newline)),
        delimited(
            token(Token::OpenParen),
            parse_function_arglist,
            token(Token::CloseParen),
        ),
        many0(token(Token::Newline)),
        // parse function return type
        opt(tuple((
            token(Token::FnReturnType),
            alt((
                token(Token::Keyword(Box::new(Keyword::Void))),
                token(Token::Keyword(Box::new(Keyword::Float))),
            )),
        ))),
        many0(token(Token::Newline)),
        parse_block,
    ))(t)
    {
        Ok((rest, (_, _, fn_name, _, args, _, fn_type, _, stmt_vec))) => {
            Ok((
                rest,
                Some(AST::Defun(
                    // function name
                    if let Token::Identifier(name) = fn_name {
                        Box::new(Symbol(name.to_string()))
                    } else {
                        return Err(Err::Error(ParseError::new(
                            rest,
                            ErrorKind::UnexpectedToken(fn_name.clone()),
                        )));
                    },
                    Box::new(Function {
                        args: args,
                        ret_type: if let Some((_, Token::Keyword(type_name))) = fn_type {
                            Some(*type_name.clone())
                        } else {
                            None
                        },
                        body: stmt_vec,
                    }),
                )),
            ))
        }
        Err(err) => Err(err),
    }
}

/// Parses a statement in the toplevel.
fn parse_1(t: &[Token]) -> ParseResult {
    alt((
        value(None, token_type_of(Token::LineComment("".to_string()))),
        value(None, token(Token::Newline)),
        map(parse_block, |b| Some(AST::Block(b))),
        parse_statement,
        parse_function_definition,
    ))(t)
}

/// Parses all toplevel statatements from the input.
pub fn parse(t: &[Token]) -> IResult<Input, Vec<AST>, ParseError<Input>> {
    match all_consuming(many0(parse_1))(t) {
        Ok((rest, astvec)) => Ok((
            rest,
            astvec
                .into_iter()
                .filter(|o| if let None = o { false } else { true })
                .map(|o| o.unwrap())
                .collect(),
        )),
        Err(err) => Err(err),
    }
}

#[cfg(test)]
mod test_parse {
    use super::*;
    use crate::tokenize::tokenize;

    fn test_parse_1(expected: AST, string: &str) {
        println!("text: {:?}", string);
        if let Ok(("", tokens)) = tokenize(string) {
            println!("tokens: {:?}", tokens);
            match parse(&tokens) {
                Ok((&[], vec)) => {
                    assert_eq!(1, vec.len());

                    if let Some(ast) = vec.iter().nth(0) {
                        assert_eq!(*ast, expected);
                    } else {
                        println!("This test case itself is wrong....");
                        assert!(false);
                    }
                }
                err => {
                    println!("{:?}", err);
                    assert!(false);
                }
            }
        } else {
            println!("This test case itself is wrong....");
            assert!(false);
        }
    }

    #[test]
    fn test_single_term() {
        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::Float(0.0))))),
            "0.0",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::UnaryOp(
                Box::new(Operator::Minus),
                Box::new(Exp::Float(1.0)),
            ))))),
            "-1",
        );
    }

    #[test]
    fn test_binary_operators() {
        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Plus),
                Box::new(Exp::Float(1.0)),
                Box::new(Exp::Float(2.0)),
            ))))),
            "1+2",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Minus),
                Box::new(Exp::Float(10.0)),
                Box::new(Exp::Float(5.0)),
            ))))),
            "10-5",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Minus),
                Box::new(Exp::UnaryOp(
                    Box::new(Operator::Minus),
                    Box::new(Exp::Float(1.0)),
                )),
                Box::new(Exp::Float(2.0)),
            ))))),
            "-1-2",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Multiply),
                Box::new(Exp::Float(42.0)),
                Box::new(Exp::UnaryOp(
                    Box::new(Operator::Minus),
                    Box::new(Exp::Float(1.0)),
                )),
            ))))),
            "42*-1",
        );
    }

    #[test]
    fn test_nested_expressions() {
        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Multiply),
                Box::new(Exp::BinaryOp(
                    Box::new(Operator::Plus),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::Float(2.0)),
                )),
                Box::new(Exp::Float(3.0)),
            ))))),
            "(1+2)*3",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Minus),
                Box::new(Exp::BinaryOp(
                    Box::new(Operator::Plus),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::Float(2.0)),
                )),
                Box::new(Exp::Float(3.0)),
            ))))),
            "1+2-3", // (1+2)-3
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Plus),
                Box::new(Exp::Float(1.0)),
                Box::new(Exp::BinaryOp(
                    Box::new(Operator::Multiply),
                    Box::new(Exp::Float(2.0)),
                    Box::new(Exp::Float(3.0)),
                )),
            ))))),
            "1+2*3", // 1+(2*3)
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Plus),
                Box::new(Exp::BinaryOp(
                    Box::new(Operator::Minus),
                    Box::new(Exp::BinaryOp(
                        Box::new(Operator::Plus),
                        Box::new(Exp::Float(1.0)),
                        Box::new(Exp::Float(2.0)),
                    )),
                    Box::new(Exp::Float(3.0)),
                )),
                Box::new(Exp::Float(4.0)),
            ))))),
            "1+2-3+4", // ((1+2)-3)+4
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Multiply),
                Box::new(Exp::BinaryOp(
                    Box::new(Operator::Plus),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::Float(2.0)),
                )),
                Box::new(Exp::BinaryOp(
                    Box::new(Operator::Minus),
                    Box::new(Exp::Float(3.0)),
                    Box::new(Exp::Float(4.0)),
                )),
            ))))),
            "(1+2)*(3-4)",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Multiply),
                Box::new(Exp::BinaryOp(
                    Box::new(Operator::Minus),
                    Box::new(Exp::BinaryOp(
                        Box::new(Operator::Plus),
                        Box::new(Exp::Float(1.0)),
                        Box::new(Exp::UnaryOp(
                            Box::new(Operator::Minus),
                            Box::new(Exp::Float(2.0)),
                        )),
                    )),
                    Box::new(Exp::Float(3.0)),
                )),
                Box::new(Exp::Float(4.0)),
            ))))),
            "(1+-2-3)*4",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Multiply),
                Box::new(Exp::BinaryOp(
                    Box::new(Operator::Plus),
                    Box::new(Exp::Float(1.0)),
                    Box::new(Exp::BinaryOp(
                        Box::new(Operator::Minus),
                        Box::new(Exp::UnaryOp(
                            Box::new(Operator::Minus),
                            Box::new(Exp::Float(2.0)),
                        )),
                        Box::new(Exp::Float(3.0)),
                    )),
                )),
                Box::new(Exp::Float(4.0)),
            ))))),
            "(1+(-2-3))*4",
        );
    }

    #[test]
    fn test_array_access() {
        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::PostOp(
                Box::new(Operator::Access),
                Box::new(Symbol("a".to_string())),
                Box::new(Exp::Float(1.0)),
            ))))),
            "a[1]",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::PostOp(
                Box::new(Operator::Access),
                Box::new(Symbol("a".to_string())),
                Box::new(Exp::Variable(Box::new(Symbol("var".to_string())))),
            ))))),
            "a[var]",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::PostOp(
                Box::new(Operator::Access),
                Box::new(Symbol("a".to_string())),
                Box::new(Exp::BinaryOp(
                    Box::new(Operator::Mod),
                    Box::new(Exp::Variable(Box::new(Symbol("var".to_string())))),
                    Box::new(Exp::BinaryOp(
                        Box::new(Operator::Minus),
                        Box::new(Exp::Float(10.0)),
                        Box::new(Exp::Float(2.0)),
                    )),
                )),
            ))))),
            "a[var%(10-2)]",
        );
    }

    #[test]
    fn test_function_call() {
        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::InvokeFn(Box::new(
                InvokeFn(Box::new(Symbol("fnc".to_string())), vec![Exp::Float(1.0)]),
            )))))),
            "fnc(1)",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::InvokeFn(Box::new(
                InvokeFn(
                    Box::new(Symbol("fnc".to_string())),
                    vec![Exp::Float(1.0), Exp::Float(2.0), Exp::Float(3.0)],
                ),
            )))))),
            "fnc(1,2,3)",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::BinaryOp(
                Box::new(Operator::Plus),
                Box::new(Exp::InvokeFn(Box::new(InvokeFn(
                    Box::new(Symbol("fn1".to_string())),
                    vec![Exp::Float(1.0), Exp::Float(2.0), Exp::Float(3.0)],
                )))),
                Box::new(Exp::InvokeFn(Box::new(InvokeFn(
                    Box::new(Symbol("fn2".to_string())),
                    vec![Exp::Float(1.0)],
                )))),
            ))))),
            "fn1(1,2,3) + fn2(1)",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::InvokeFn(Box::new(
                InvokeFn(
                    Box::new(Symbol("f1".to_string())),
                    vec![Exp::InvokeFn(Box::new(InvokeFn(
                        Box::new(Symbol("f2".to_string())),
                        vec![Exp::Float(42.0)],
                    )))],
                ),
            )))))),
            "f1(f2(42))",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::InvokeFn(Box::new(
                InvokeFn(
                    Box::new(Symbol("f1".to_string())),
                    vec![
                        Exp::InvokeFn(Box::new(InvokeFn(
                            Box::new(Symbol("f2".to_string())),
                            vec![Exp::Float(10.0)],
                        ))),
                        Exp::InvokeFn(Box::new(InvokeFn(
                            Box::new(Symbol("f3".to_string())),
                            vec![Exp::Float(20.0)],
                        ))),
                    ],
                ),
            )))))),
            "f1(f2(10), f3(20))",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::InvokeFn(Box::new(
                InvokeFn(
                    Box::new(Symbol("f1".to_string())),
                    vec![
                        Exp::InvokeFn(Box::new(InvokeFn(
                            Box::new(Symbol("f2".to_string())),
                            vec![Exp::Float(10.0)],
                        ))),
                        Exp::InvokeFn(Box::new(InvokeFn(
                            Box::new(Symbol("f3".to_string())),
                            vec![Exp::Float(20.0)],
                        ))),
                    ],
                ),
            )))))),
            "f1(f2(10), f3(20))",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::InvokeFn(Box::new(
                InvokeFn(
                    Box::new(Symbol("f1".to_string())),
                    vec![Exp::BinaryOp(
                        Box::new(Operator::Plus),
                        Box::new(Exp::Float(1.0)),
                        Box::new(Exp::Float(2.0)),
                    )],
                ),
            )))))),
            "f1(1+2)",
        );

        test_parse_1(
            AST::Statement(Box::new(Statement::Exp(Box::new(Exp::InvokeFn(Box::new(
                InvokeFn(
                    Box::new(Symbol("f1".to_string())),
                    vec![
                        Exp::BinaryOp(
                            Box::new(Operator::Plus),
                            Box::new(Exp::Float(1.0)),
                            Box::new(Exp::Float(2.0)),
                        ),
                        Exp::BinaryOp(
                            Box::new(Operator::Multiply),
                            Box::new(Exp::Float(3.0)),
                            Box::new(Exp::Float(4.0)),
                        ),
                    ],
                ),
            )))))),
            "f1(1+2, 3*4)",
        );
    }

    #[test]
    fn test_empty_string() {
        if let Ok(("", tokens)) = tokenize("") {
            println!("tokens: {:?}", tokens);
            match parse(&tokens) {
                Ok((&[], vec)) => {
                    assert_eq!(vec.len(), 0);
                }
                Ok((_, _)) => assert!(false),
                Err(err) => {
                    println!("err = {:?}", err);
                    assert!(false)
                }
            }
        } else {
            assert!(false)
        }
    }

    fn test_parse_all(expected: &[AST], string: &str) {
        println!("text: {:?}", string);
        if let Ok(("", tokens)) = tokenize(string) {
            println!("tokens: {:?}", tokens);
            match parse(&tokens) {
                Ok((&[], vec)) => {
                    assert_eq!(vec.len(), expected.len());
                    for (i, ast) in vec.iter().enumerate() {
                        assert_eq!(*ast, expected[i]);
                    }
                }
                err => {
                    println!("{:?}", err);
                    assert!(false);
                }
            }
        } else {
            println!("This test case itself is wrong....");
            assert!(false);
        }
    }

    #[test]
    fn test_assignment() {
        test_parse_all(
            &[AST::Statement(Box::new(Statement::Assign(
                Box::new(Symbol("var".to_string())),
                Box::new(Exp::Float(123.0)),
            )))],
            "var = 123",
        );

        test_parse_all(
            &[
                AST::Statement(Box::new(Statement::Assign(
                    Box::new(Symbol("var".to_string())),
                    Box::new(Exp::Float(123.0)),
                ))),
                AST::Statement(Box::new(Statement::Assign(
                    Box::new(Symbol("var2".to_string())),
                    Box::new(Exp::Float(456.0)),
                ))),
            ],
            "var = 123
             var2 = 456",
        );

        test_parse_all(
            &[
                AST::Statement(Box::new(Statement::Assign(
                    Box::new(Symbol("var".to_string())),
                    Box::new(Exp::Float(123.0)),
                ))),
                AST::Statement(Box::new(Statement::Assign(
                    Box::new(Symbol("var2".to_string())),
                    Box::new(Exp::BinaryOp(
                        Box::new(Operator::Plus),
                        Box::new(Exp::Float(4.0)),
                        Box::new(Exp::BinaryOp(
                            Box::new(Operator::Multiply),
                            Box::new(Exp::Float(5.0)),
                            Box::new(Exp::Float(6.0)),
                        )),
                    )),
                ))),
            ],
            "var = 123
             var2 = 4+5*6",
        );

        test_parse_all(
            &[AST::Statement(Box::new(Statement::Assign(
                Box::new(Symbol("var".to_string())),
                Box::new(Exp::InvokeFn(Box::new(InvokeFn(
                    Box::new(Symbol("f".to_string())),
                    vec![Exp::BinaryOp(
                        Box::new(Operator::Plus),
                        Box::new(Exp::Float(1.0)),
                        Box::new(Exp::Float(2.0)),
                    )],
                )))),
            )))],
            "var = f(1+2)",
        );
    }

    #[test]
    fn test_function_definition() {
        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("func".to_string())),
                vec![],
                None,
                vec![],
            )],
            "fn func() {}",
        );

        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("func".to_string())),
                vec![],
                None,
                vec![Statement::Assign(
                    Box::new(Symbol("a".to_string())),
                    Box::new(Exp::Float(42.0)),
                )],
            )],
            "fn func() {a=42}",
        );

        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("func".to_string())),
                vec![],
                None,
                vec![
                    Statement::Assign(
                        Box::new(Symbol("a".to_string())),
                        Box::new(Exp::Float(42.0)),
                    ),
                    Statement::Assign(
                        Box::new(Symbol("b".to_string())),
                        Box::new(Exp::Float(84.0)),
                    ),
                ],
            )],
            "fn func() {a=42\nb=84}",
        );
    }

    #[test]
    fn test_function_args() {
        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("func".to_string())),
                vec![Declare::Var(Box::new(Symbol("a".to_string())), None)],
                None,
                vec![],
            )],
            "fn func(a) {}",
        );

        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("func".to_string())),
                vec![
                    Declare::Var(Box::new(Symbol("a".to_string())), None),
                    Declare::Var(Box::new(Symbol("b".to_string())), None),
                ],
                None,
                vec![],
            )],
            "fn func(a, b) {}",
        );

        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("func".to_string())),
                vec![Declare::Var(
                    Box::new(Symbol("a".to_string())),
                    Some(Keyword::Float),
                )],
                None,
                vec![],
            )],
            "fn func(a: float) {}",
        );

        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("func".to_string())),
                vec![
                    Declare::Var(Box::new(Symbol("a".to_string())), None),
                    Declare::Var(Box::new(Symbol("b".to_string())), Some(Keyword::Void)),
                ],
                None,
                vec![],
            )],
            "fn func(a, b: void) {}",
        );

        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("func".to_string())),
                vec![
                    Declare::Var(Box::new(Symbol("a".to_string())), Some(Keyword::Float)),
                    Declare::Var(Box::new(Symbol("b".to_string())), None),
                ],
                None,
                vec![],
            )],
            "fn func(a: float, b) {}",
        );

        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("func".to_string())),
                vec![
                    Declare::Var(Box::new(Symbol("a".to_string())), Some(Keyword::Float)),
                    Declare::Var(Box::new(Symbol("b".to_string())), Some(Keyword::Void)),
                ],
                None,
                vec![],
            )],
            "fn func(a: float, b: void) {}",
        );
    }

    #[test]
    fn test_return() {
        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("func".to_string())),
                vec![],
                None,
                vec![Statement::Return(Box::new(Exp::Float(0.0)))],
            )],
            "fn func() {return 0}",
        );

        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("f1".to_string())),
                vec![],
                None,
                vec![Statement::Return(Box::new(Exp::BinaryOp(
                    Box::new(Operator::Plus),
                    Box::new(Exp::Float(10.0)),
                    Box::new(Exp::InvokeFn(Box::new(InvokeFn(
                        Box::new(Symbol("f".to_string())),
                        vec![Exp::BinaryOp(
                            Box::new(Operator::Minus),
                            Box::new(Exp::Float(20.0)),
                            Box::new(Exp::Float(1.0)),
                        )],
                    )))),
                )))],
            )],
            "fn f1() {return 10+f(20-1)}",
        );
    }

    #[test]
    fn test_function_return_type() {
        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("loop".to_string())),
                vec![],
                Some(Keyword::Void),
                vec![],
            )],
            "fn loop()->void {}",
        );

        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("loop".to_string())),
                vec![],
                Some(Keyword::Void),
                vec![],
            )],
            "fn loop()->void{}",
        );
    }

    #[test]
    fn test_invoke_function_at() {
        test_parse_all(
            &[AST::Statement(Box::new(Statement::InvokeAt(
                Box::new(InvokeFn(Box::new(Symbol("test".to_string())), vec![])),
                Box::new(Exp::Float(10.0)),
            )))],
            "test()@10",
        );

        test_parse_all(
            &[AST::Statement(Box::new(Statement::InvokeAt(
                Box::new(InvokeFn(Box::new(Symbol("test".to_string())), vec![])),
                Box::new(Exp::BinaryOp(
                    Box::new(Operator::Plus),
                    Box::new(Exp::Float(10.0)),
                    Box::new(Exp::Float(1.0)),
                )),
            )))],
            "test()@(10+1)",
        );

        test_parse_all(
            &[AST::Statement(Box::new(Statement::InvokeAt(
                Box::new(InvokeFn(Box::new(Symbol("test".to_string())), vec![])),
                Box::new(Exp::InvokeFn(Box::new(InvokeFn(
                    Box::new(Symbol("rand".to_string())),
                    vec![],
                )))),
            )))],
            "test()@(rand())",
        );
    }

    #[test]
    fn test_newlines() {
        test_parse_all(
            &[AST::Defun(
                Box::new(Symbol("loop".to_string())),
                vec![],
                Some(Keyword::Void),
                vec![],
            )],
            "fn loop()\n->void {}",
        );
    }

    #[test]
    fn test_if() {
        test_parse_all(
            &[AST::Statement(Box::new(Statement::Exp(Box::new(Exp::If(
                Box::new(If {
                    cond: Box::new(Exp::Variable(Box::new(Symbol("a".to_string())))),
                    true_clause: vec![Exp::Variable(Box::new(Symbol("b".to_string())))],
                    false_clause: Some(vec![Exp::Variable(Box::new(Symbol("c".to_string())))]),
                }),
            )))))],
            "if (a) b else c",
        );

        test_parse_all(
            &[AST::Statement(Box::new(Statement::Exp(Box::new(Exp::If(
                Box::new(If {
                    cond: Box::new(Exp::BinaryOp(
                        Box::new(Operator::Mod),
                        Box::new(Exp::BinaryOp(
                            Box::new(Operator::Plus),
                            Box::new(Exp::Variable(Box::new(Symbol("a".to_string())))),
                            Box::new(Exp::Variable(Box::new(Symbol("b".to_string())))),
                        )),
                        Box::new(Exp::InvokeFn(Box::new(InvokeFn(
                            Box::new(Symbol("c".to_string())),
                            Vec::new(),
                        )))),
                    )),
                    true_clause: vec![Exp::Float(0.0)],
                    false_clause: Some(vec![Exp::InvokeFn(Box::new(InvokeFn(
                        Box::new(Symbol("f".to_string())),
                        Vec::new(),
                    )))]),
                }),
            )))))],
            "if ((a + b) % c()) {0} else {f()}",
        );
    }
}

﻿module Parser

open Lexer

type Operation = | Plus | Minus | Times
let private parseOperation = function
| "+" -> Some Plus
| "-" -> Some Minus
| "*" -> Some Times
| _   -> None 

type Function =
    | Builtin of Operation

type Expression = 
    | ConstantInt of int
    | Defun       of string   * Expression list
    | Invoke      of Function * Expression list
    | Error       of string

let rec findAllErrors = function
| Defun       (_, expressions) -> expressions |> List.collect findAllErrors
| Invoke      (_, expressions) -> expressions |> List.collect findAllErrors
| ConstantInt _ -> []
| Error message -> [ message ]

type private ParseState = {
    Expressions: Expression list
    Remaining:   Lexeme list 
}

let private error (state : ParseState, message: string): ParseState =
    { state with Expressions = state.Expressions @ [ Error message ] }

let rec private parseExpression (state : ParseState): ParseState =
    match state.Remaining with
    | LeftParenthesis     :: Identifier name    :: rest -> parseInvoke (name, { state with Remaining = rest })
    | LeftParenthesis     :: wrong -> error (state, sprintf "%A cannot follow '('." wrong) 
    | RightParenthesis    :: _     -> error (state, "Unmatched )")
    | Identifier   name   :: _     -> error (state, sprintf "Unrecognized identifier '%s'." name) 
    | LiteralInt   number :: rest  ->  
        { Expressions = state.Expressions @ [ ConstantInt number ]; Remaining = rest }
    | Unrecognized char   :: _    -> error (state, sprintf "Unexpected character %A" char )
    | [] -> state
and private parseInvoke (identifier: string, state : ParseState) =
    let arguments = parseArguments { state with Expressions = [] }
    match parseOperation identifier with
    | Some operation -> 
        parseExpression { Expressions = state.Expressions @ [ Invoke(Builtin operation, arguments.Expressions) ]; Remaining = arguments.Remaining }
    | None -> error (state, sprintf "Unknown function '%s'." identifier) 
and private parseArguments (state : ParseState) : ParseState =
    match state.Remaining with 
    | [] -> error (state, "')' expected.")
    | RightParenthesis :: rest -> { state with Remaining = rest }
    | _ -> parseArguments (parseExpression state)

let rec private parseExpressions (state : ParseState): ParseState =
    let parsed = parseExpression state
    match parsed.Remaining with
    | [] -> parsed
    | _  -> parseExpressions parsed

let rec private containsMain expressions = 
    match expressions with 
    | Defun ("main", _) :: _-> true
    | [] -> false
    | _ :: rest -> containsMain rest

let private ensureHasMainFunction = function
    | [] -> []
    | expressions when containsMain expressions -> expressions
    | [ expr ]    -> [ Defun("main", [ expr ]) ]
    | expressions -> expressions @ [ Error "No main function found." ]

let parse (lexemes: Lexeme list): Expression list=
    let parsed = parseExpressions { Expressions = []; Remaining = lexemes }
    parsed.Expressions |> ensureHasMainFunction
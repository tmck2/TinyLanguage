﻿module Lexer

type Lexeme =
    | LeftParenthesis
    | RightParenthesis
    | Identifier   of string
    | LiteralInt   of int
    | Unrecognized of char

let private isIdentifierStart (c: char) =
    System.Char.IsLetter c || System.Char.IsPunctuation c || System.Char.IsSymbol c

let private isIdentifierBody (c: char) =
    System.Char.IsLetter c || System.Char.IsPunctuation c || System.Char.IsSymbol c || System.Char.IsDigit c

let rec private lexChars (source: char list): Lexeme list =
    match source with
    | '(' :: rest -> LeftParenthesis  :: lexChars rest
    | ')' :: rest -> RightParenthesis :: lexChars rest
    | c   :: _ when isIdentifierStart c   -> lexName(source, "")
    | d   :: _ when System.Char.IsDigit d -> lexInteger(source, "")
    | []          -> []
    | w   :: rest when System.Char.IsWhiteSpace w -> lexChars rest
    | c   :: rest -> Unrecognized c   :: lexChars rest
and lexInteger(source: char list, number: string) =
    match source with
    | d :: rest when System.Char.IsDigit d -> lexInteger(rest, number + d.ToString())
    | _ -> LiteralInt (System.Int32.Parse number) :: lexChars source
and lexName(source: char list, name: string) =
    match source with
    | c :: rest when isIdentifierStart c && name = "" -> lexName(rest, name + c.ToString())
    | c :: rest when isIdentifierBody  c              -> lexName(rest, name + c.ToString())
    | _ -> Identifier name :: lexChars source

let lex (source: string): Lexeme list =
    List.ofSeq source |> lexChars
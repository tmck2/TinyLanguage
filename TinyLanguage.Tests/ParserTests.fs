﻿namespace TinyLangage.Tests.Lexer

open NUnit.Framework
open FsUnit
open Lexer
open Parser
open Syntax
open TestHelpers

type ParserTests () =
    [<Test>] 
    member this.``should parse nothing``() = 
        parse [] |> should equal []

    [<Test>] 
    member this.``should parse (inc 2)``() = 
        parse [ LeftParenthesis; Identifier "inc"; LiteralInt 2; RightParenthesis] 
            |> should equal 
            [ InvokeExpr("inc", Some (IntExpr 2)) ]

    [<Test>] 
    member this.``missing right parenthesis should error``() = 
        NUnit.Framework.Assert.That(
            parse [ LeftParenthesis; Identifier "inc"; LiteralInt 2] 
            |> List.exists (isTreeWithErrorMessageContaining "Expected ')'"))

    [<Test>] 
    member this.``should parse defun with int argument``() = 
        let actual = parse [ LeftParenthesis; Identifier "defun"; Identifier "add-1"; LeftParenthesis; Identifier "int"; Identifier "x"; RightParenthesis; LeftParenthesis; Identifier "inc"; Identifier "x"; RightParenthesis; RightParenthesis ] 
        actual |> should equal
            [ DefunExpr("add-1", Some { TypeName = "int"; ArgumentName = "x" }, InvokeExpr("inc", Some(IdentifierExpr "x")))]
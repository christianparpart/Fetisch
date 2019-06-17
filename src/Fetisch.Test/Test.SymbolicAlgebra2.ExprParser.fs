// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module SymbolicAlgebra2.ExprParser

open Xunit
open Fetisch.Experimental.Symbolics
open Fetisch.Experimental.Symbolics.ExprParser
open Fetisch.Experimental.Symbolics.Operations

[<Fact>]
let ``parse: number`` () =
    let x = tokenizeString "1"
    Assert.Equal(2, List.length x)
    Assert.Equal(NumberLiteral(1I), List.item 0 x)
    Assert.Equal(Eof, List.item 1 x)

[<Fact>]
let ``parse: symbol`` () =
    let x = tokenizeString "a bc def"
    Assert.Equal(4, List.length x)
    Assert.Equal(Identifier("a"), List.item 0 x)
    Assert.Equal(Identifier("bc"), List.item 1 x)
    Assert.Equal(Identifier("def"), List.item 2 x)
    Assert.Equal(Eof, List.item 3 x)

[<Fact>]
let ``parse: braces`` () =
    let x = tokenizeString "()"
    Assert.Equal(3, List.length x)
    Assert.Equal(RndOpen, List.item 0 x)
    Assert.Equal(RndClose, List.item 1 x)
    Assert.Equal(Eof, List.item 2 x)

[<Fact>]
let ``parse: ops`` () =
    let x = tokenizeString "+ - * / **"
    Assert.Equal(Token.Plus, List.item 0 x)
    Assert.Equal(Token.Minus, List.item 1 x)
    Assert.Equal(Token.Mul, List.item 2 x)
    Assert.Equal(Token.Div, List.item 3 x)
    Assert.Equal(Token.Pow, List.item 4 x)
    Assert.Equal(Token.Eof, List.item 5 x)

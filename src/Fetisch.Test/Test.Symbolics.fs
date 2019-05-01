// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module SymbolicAlgebra.Experimental

open Xunit
open Fetisch.Experimental.Symbolics
open Fetisch.Experimental.Symbolics.Operations

let a, b, c = var "a", var "b", var "c"

[<Fact>]
let ``add: constant folding`` () =
    Assert.Equal("5", (2G + 3G).ToString())

    Assert.Equal("9", (2G + (3G + 4G)).ToString())
    Assert.Equal("9", ((2G + 3G) + 4G).ToString())

    Assert.Equal("6 + a", ((2G + a) + 4G).ToString())

[<Fact>]
let ``add: var`` () =
    Assert.Equal("5 + a", (5G + a).ToString())
    Assert.Equal("5 + a", (a + 5G).ToString())

[<Fact>]
let ``add: neutral element`` () =
    Assert.Equal("a", (0G + a).ToString())
    Assert.Equal("a", (a + 0G).ToString())

[<Fact>]
let ``add: inverse element`` () =
    Assert.Equal("0", (a + (-a)).ToString())
    Assert.Equal("0", ((-a) + a).ToString())

[<Fact>]
let ``mul: constant folding`` () =
    Assert.Equal("12", (3G * 4G).ToString())

[<Fact>]
let ``expr tokenizer`` () =
    let t = ExprParser.tokenizeString "12 * (34 + 5)"
    Assert.Equal(8, List.length t)
    Assert.Equal(Token.NumberLiteral(12I), List.item 0 t)
    Assert.Equal(Token.Mul, List.item 1 t)
    Assert.Equal(Token.RndOpen, List.item 2 t)
    Assert.Equal(Token.NumberLiteral(34I), List.item 3 t)
    Assert.Equal(Token.Plus, List.item 4 t)
    Assert.Equal(Token.NumberLiteral(5I), List.item 5 t)
    Assert.Equal(Token.RndClose, List.item 6 t)
    Assert.Equal(Token.Eof, List.item 7 t)


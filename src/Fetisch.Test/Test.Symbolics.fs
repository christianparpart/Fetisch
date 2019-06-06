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

let e2s (e: Expression): string =
    e.ToString()

[<Fact>]
let ``orderRelation`` () =
    let a = 2G
    let b = 3G * Variable("a")
    let c = Variable("a") ** 3G
    let r1 = Operations.orderRelation a b
    let r2 = Operations.orderRelation b c
    let r3 = Operations.orderRelation a c
    Assert.True(r1)
    Assert.True(r2)
    Assert.True(r3)

[<Fact>]
let ``add: constant folding`` () =
    Assert.Equal("5", e2s (2G + 3G))
    Assert.Equal("9", e2s (2G + (3G + 4G)))
    Assert.Equal("9", e2s ((2G + 3G) + 4G))
    Assert.Equal("6 + a", e2s ((2G + a) + 4G))
    Assert.Equal("6 + a", e2s (2G + (a + 4G)))

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
let ``mul: factor ordering`` () =
    Assert.Equal("2 * a", e2s (2G * a))
    Assert.Equal("2 * a", e2s (a * 2G))

[<Fact>]
let ``mul: constant folding`` () =
    Assert.Equal("6", e2s (2G * 3G))
    Assert.Equal("24", e2s (2G * (3G * 4G)))
    Assert.Equal("24", e2s ((2G * 3G) * 4G))
    Assert.Equal("8 * a", e2s (2G * (4G * a)))
    Assert.Equal("8 * a", e2s (2G * (a * 4G)))
    Assert.Equal("8 * a", e2s ((2G * a) * 4G))

let ``mul: optimizations`` () =
    Assert.Equal("a", e2s (1G * a))
    Assert.Equal("0", e2s (0G * a))

let ``proper merging`` () =
    let a = Variable "a"
    let b = Variable "b"
    let lhs = a**2G + a * b
    let rhs = a * b + b ** 2G
    let res = lhs + rhs
    Assert.Equal("", res.ToString())

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

[<Fact>]
let ``expr parser`` () =
    // TODO
    Assert.True(true)


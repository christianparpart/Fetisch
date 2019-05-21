// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module SymbolicAlgebra.ExprMatcher

open Xunit
open Fetisch.SymbolicAlgebra
open Fetisch.SymbolicAlgebra.ExprMatcher

let a, b = Variable("a"), Variable("b")
let x, y = Variable("x"), Variable("y")

[<Fact>]
let ``match: basic`` () =
    let pattern = (a + b) * (a + b)
    let expr = (x + 2G*y) * (x + 2G*y)
    let ml = (ExprMatcher.matchRule expr pattern).Value
    Assert.Equal(2, List.length ml)
    Assert.Contains(ml, fun (m: Match) -> m.Name = "a" && m.Expression = x)
    Assert.Contains(ml, fun (m: Match) -> m.Name = "b" && m.Expression = 2G * y)

[<Fact>]
let ``match: conflicting names`` () =
    let pattern = a + a
    let expr = (a * b) + (a * b)
    let ml = (ExprMatcher.matchRule expr pattern).Value
    Assert.Equal(1, List.length ml)
    Assert.Contains(ml, fun (m: Match) -> m.Name = "a" && m.Expression = a * b)

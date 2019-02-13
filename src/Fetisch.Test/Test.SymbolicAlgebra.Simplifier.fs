// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module SymbolicAlgebra.Simplifier

open Xunit
open Fetisch.SymbolicAlgebra
open Fetisch.SymbolicAlgebra.Simplifier

let a, b, c = SymbolExpr("a"), SymbolExpr("b"), SymbolExpr("c")

[<Fact>]
let ``constant folding: flat`` () =
    Assert.Equal(3G, constantFold (-(-(3G))))
    Assert.Equal(3G, constantFold (AbsExpr(-3G)))
    Assert.Equal(5G, constantFold (3G + 2G))
    Assert.Equal(1G, constantFold (3G - 2G))
    Assert.Equal(6G, constantFold (3G * 2G))
    Assert.Equal(2G, constantFold (6G / 3G))
    Assert.Equal(8G, constantFold (2G ^^ 3G))
    Assert.Equal(3G, constantFold 3G)
    Assert.Equal(SymbolExpr("x"), constantFold (SymbolExpr("x")))

[<Fact>]
let ``constant folding: tree`` () =
    Assert.Equal(20G, constantFold ((2G + 3G) * 4G))
    Assert.Equal(14G, constantFold (2G + 3G * 4G))

[<Fact>]
let ``simplify: scalars left`` () =
    Assert.Equal(2G * a, simplify (a * 2G))

[<Fact>]
let ``simplify: consts right`` () =
    Assert.Equal(a + 2G, simplify (2G + a))

[<Fact>]
let ``simplify: addition`` () =
    Assert.Equal(a, simplify (a + 0G))
    Assert.Equal(a, simplify (0G + a))
    Assert.Equal(0G, simplify (a - a))

[<Fact>]
let ``simplify: multiplication`` () =
    Assert.Equal(a, simplify (a * 1G))
    Assert.Equal(a, simplify (1G * a))
    Assert.Equal(1G, simplify (a / a))
    Assert.Equal(a ^^ 2G, simplify (a * a))

[<Fact>]
let ``simplify: distributivity`` () =
    Assert.Equal(a * (b + c), simplify (a*b + a*c))
    Assert.Equal((a + b) * c, simplify (a*c + b*c))

[<Fact>]
let ``simplify: exp`` () =
    Assert.Equal(1G, simplify (a ^^ 0G))
    Assert.Equal(a, simplify (a ^^ 1G))
    Assert.Equal(a ^^ (b + 1G), simplify (a * (a ^^ b)))
    Assert.Equal(a ^^ (b * c), simplify ((a ^^ b) ^^ c))

[<Fact>]
let ``simplify: misc`` () =
    Assert.Equal((a ^^ 2G) - (b ^^ 2G), simplify ((a + b) * (a - b)))
    Assert.Equal((a + b) ^^ 2G, simplify ((a ^^ 2G) + (2G*a*b) + (b ^^ 2G)))
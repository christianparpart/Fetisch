// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module BigRational

open System
open Xunit
open Fetisch

let inline f a b =
    BigRational.FromIntFraction(a, b)

[<Fact>]
let ``creation & ops`` () =
    let a = 2N
    Assert.Equal(2N, a)
    Assert.Equal(2I, a.Numerator)
    Assert.Equal(1I, a.Denominator)

[<Fact>]
let ``add`` () =
    // a/b + c/d = (ad + bc) / bd
    Assert.Equal(f 5 4, (f 1 2) + (f 3 4))

[<Fact>]
let ``sub`` () =
    let a = 2N
    Assert.Equal(0N, a - a)

[<Fact>]
let ``mul`` () =
    // a/b * c/d
    Assert.Equal(f 3 10, (f 1 2) * (f 3 5))

[<Fact>]
let ``div`` () =
    Assert.Equal(1N, a / a)
    Assert.Equal(1N / 2N, (a / a) / a)

[<Fact>]
let ``pow`` () =
    let a = BigRational.FromIntFraction(1, 2)
    let b = BigRational.FromIntFraction(3, 4)

    // (a/b)^0 = 1
    Assert.Equal(1N, 5N ** BigRational.Zero)

    // (a/b)^1 = a/b
    Assert.Equal(5N, 5N ** BigRational.One)

    // (a/b)^(-a/b) = 1
    Assert.Equal(BigRational.One, (a/b) ** (-a/b))

[<Fact>]
let ``abs`` () =
    Assert.Equal(2N, abs -2N)

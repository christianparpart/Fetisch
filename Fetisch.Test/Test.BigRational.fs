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

[<Fact>]
let ``creation & ops`` () =
    let a = 2N
    Assert.Equal(2N, a)
    Assert.Equal(2I, a.Nominator)
    Assert.Equal(1I, a.Denominator)

[<Fact>]
let ``ops`` () =
    let a = 2N
    Assert.Equal(0N, a - a)
    Assert.Equal(1N, a / a)
    Assert.Equal(1N / 2N, (a / a) / a)

    Assert.Equal(2N, abs -2N)

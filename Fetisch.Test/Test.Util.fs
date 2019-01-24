// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module Util

open System
open Xunit
open Fetisch
open Fetisch.Util

[<Fact>]
let ``isOne`` () =
    Assert.True(isOne 1)
    Assert.True(isOne 1N)
    Assert.True(isOne 1I)

[<Fact>]
let ``isZero`` () =
    Assert.True(isZero 0)
    Assert.True(isZero 0.0)
    Assert.True(isZero 0I)
    Assert.True(isZero 0N)
    Assert.False(isZero 1N)

[<Fact>]
let ``isNotOne`` () =
    Assert.True(isNotOne 2)
    Assert.True(isNotOne 2.0)
    Assert.True(isNotOne 2I)
    Assert.True(isNotOne 2N)
    Assert.False(isNotOne 1N)

[<Fact>]
let ``isNotZero`` () =
    Assert.True(isNotZero 2)
    Assert.True(isNotZero 2.0)
    Assert.True(isNotZero 2I)
    Assert.True(isNotZero 2N)
    Assert.True(isNotZero -1N)
    Assert.False(isNotZero 0N)

[<Fact>]
let ``isMinusOne`` () =
    Assert.True(isMinusOne -1)
    Assert.True(isMinusOne -1.0)
    Assert.True(isMinusOne -1I)
    Assert.True(isMinusOne -1N)
    Assert.False(isMinusOne 0N)

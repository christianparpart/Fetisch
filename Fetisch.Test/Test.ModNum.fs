// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module ModNum

open System
open Xunit
open Fetisch.ModNum

[<Fact>]
let ``creation & access`` () =
    let a = ModNum(2, 4)
    Assert.Equal(2, int a)
    Assert.Equal(4, int a.Mod)

    let one = ModNum.One
    Assert.Equal(1, int one)

    let zero = ModNum.Zero
    Assert.Equal(0, int zero)

[<Fact>]
let ``ops: addition`` () =
    let a = ModNum(2, 4)
    let b = ModNum(3, 4)
    let c = a + b
    Assert.Equal(ModNum(1, 4), c)

[<Fact>]
let ``ops: multiplication`` () =
    let a = ModNum(2, 4)
    let b = ModNum(3, 4)
    Assert.Equal(ModNum(2, 4), a * b)
    Assert.Equal(ModNum(0, 4), a * a)


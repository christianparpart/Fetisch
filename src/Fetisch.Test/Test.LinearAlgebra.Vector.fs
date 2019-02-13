// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module LinearAlgebra.Vector

open System
open Xunit
open Fetisch.LinearAlgebra

[<Fact>]
let ``creation & access`` () =
    let v = Vector [| 7; 3; 5 |]
    Assert.Equal(3, v.Dimension)
    Assert.Equal(7, v.[1])
    Assert.Equal(3, v.[2])
    Assert.Equal(5, v.[3])

[<Fact>]
let ``scalar multiplication`` () =
    let v =  Vector [| 1; 2; 3 |]
    Assert.Equal(Vector [| 2; 4; 6 |], 2 * v)


// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module LinearAlgebra.Matrix

open System
open Xunit
open Fetisch.LinearAlgebra

[<Fact>]
let ``creation & access`` () =
    let m = Matrix.create [[1; 2; 3]; [4; 5; 6]]

    Assert.Equal(2, Matrix.rowCount m)
    Assert.Equal(3, Matrix.columnCount m)

    Assert.Equal(Vector [|1; 2; 3|], m.Row 1)
    Assert.Equal(Vector [|4; 5; 6|], m.Row 2)

    Assert.Equal(Vector [|1; 4|], m.Column 1)
    Assert.Equal(Vector [|2; 5|], m.Column 2)
    Assert.Equal(Vector [|3; 6|], m.Column 3)

[<Fact>]
let ``multiply by scalar`` () =
    let m = Matrix.init 2 2 (fun i j -> if i = j then 1 else 0)
    // let m' = 2 * m
    let m' = m * 2
    Assert.Equal(2, m'.[1, 1])
    Assert.Equal(2, m'.[2, 2])
    Assert.Equal(0, m'.[1, 2])
    Assert.Equal(0, m'.[2, 1])

[<Fact>]
let ``matrix multiplication`` () =
    let m = Matrix.create [[-1; 2; 2]; [0;-1;-2]; [2;2;1]]
    let n = Matrix.create [[ 2; 3; 4]; [1; 2; 3]; [4;4;2]]
    let o = m * n
    o |> ignore // TODO

[<Fact>]
let ``complement`` () =
    let m = Matrix.create [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]]
    let a = Matrix.complement m 2 2
    Assert.Equal(Vector [| 1; 3 |], Matrix.row a 1)
    Assert.Equal(Vector [| 7; 9 |], Matrix.row a 2)

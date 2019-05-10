// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module LinearAlgebra.Solver

open Xunit
open Fetisch.LinearAlgebra

[<Fact>]
let ``simple`` () =
    let mul (a: Matrix<BigRational>) (b: Matrix<BigRational>): Matrix<BigRational> = a * b
    let A = Matrix.createQ [[1; 2; 3]; [4; 5; 6]; [7; 8; 9]];
    let I = Matrix.init 3 3 (fun i j -> if i = j then 1N else 0N)
    let C = Solver.decompose A
    let A' = List.fold mul I C
    let A_times_A' = A * A'
    printfn "A    : %s" (A.AsString())
    printfn "A'   : %s" (A'.AsString())
    printfn "A*A' : %s" (A_times_A'.AsString())
    Assert.True(true) // TODO
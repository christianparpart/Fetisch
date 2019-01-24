// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module Permutation

open System
open Xunit
open Fetisch
open Fetisch.Algebra

[<Fact>]
let ``identity`` () =
    let a = Permutation.identity 5
    Assert.Equal("(5) (4) (3) (2) (1)", a.ToString())

[<Fact>]
let ``ofTransposition`` () =
    let a = Permutation.ofTransposition 4 (2, 3)
    Assert.Equal(1, a.Map(1))
    Assert.Equal(2, a.Map(3))
    Assert.Equal(3, a.Map(2))
    Assert.Equal(4, a.Map(4))

[<Fact>]
let ``ofTranspositions`` () =
    let a = Permutation.ofTranspositions 6 [(5, 1); (1, 2); (2, 5); (3, 4); (4, 3)]
    Assert.Equal("(6) (5 1 2) (4 3)", a.ToString())

[<Fact>]
let ``failureCount`` () =
    let a = Permutation.ofList [3; 2; 4; 1]
    Assert.Equal(4, Permutation.failureCount a)

//[<Fact>]
//let ``ofCycles`` () =
//    let b = Permutation.ofCycles 5 [[5; 1; 2]; [3; 4]]
//    Assert.Equal("(5 1 2) (4 3)", b.ToString())

[<Fact>]
let ``ofList`` () =
    let a = Permutation.ofList [2; 3; 1; 4]
    Assert.Equal("(4) (3 1 2)", a.ToCanonicalCycleForm())

    let b = Permutation.ofList [3; 2; 4; 1]
    Assert.Equal("(4 1 3) (2)", b.ToCanonicalCycleForm())

[<Fact>]
let ``toSimpleCyclicForm`` () =
    let a = Permutation.identity 3
    Assert.Equal("(1 1) (2 2) (3 3)", a.ToSimpleCycleForm())

[<Fact>]
let ``ofList: invalid input`` () =
    // value too big
    Assert.Throws<ArgumentException>(fun () -> Permutation.ofList [2] |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> Permutation.ofList [0; 1] |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> Permutation.ofList [1; 2; 4] |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> Permutation.ofList [1; 2; 7] |> ignore) |> ignore

    // value too small
    Assert.Throws<ArgumentException>(fun () -> Permutation.ofList [0] |> ignore) |> ignore
    Assert.Throws<ArgumentException>(fun () -> Permutation.ofList [-1] |> ignore) |> ignore

    // combination of both
    Assert.Throws<ArgumentException>(fun () -> Permutation.ofList [0; 2] |> ignore) |> ignore

[<Fact>]
let ``composition`` () =
    let a = Permutation.ofList [2; 3; 1; 4]
    let b = Permutation.ofList [3; 2; 4; 1]
    let composed = a * b
    Assert.Equal("(1 1) (2 3) (3 4) (4 2)", composed.ToSimpleCycleForm())

[<Fact>]
let ``inverse`` () =
    let a = Permutation.ofList [3; 4; 5; 6; 1; 2]
    Assert.Equal("(1 3) (2 4) (3 5) (4 6) (5 1) (6 2)", a.ToSimpleCycleForm())

    let b = Permutation.inverse a
    Assert.Equal("(1 5) (2 6) (3 1) (4 2) (5 3) (6 4)", b.ToSimpleCycleForm())

let private contains (s: int list) (l: Permutation list) : bool =
    let f (p: Permutation) = (Array.toList p.Values).Tail = s
    List.exists f l

[<Fact>]
let ``all: 1`` () =
    let a = Permutation.all 1
    Assert.Equal(1, List.length a)
    Assert.True(contains [1] a)

[<Fact>]
let ``all: 2`` () =
    let b = Permutation.all 2
    Assert.Equal(2, List.length b)
    Assert.True(contains [1; 2] b)
    Assert.True(contains [2; 1] b)

[<Fact>]
let ``all: 3`` () =
    let c = Permutation.all 3
    Assert.Equal(6, List.length c)
    Assert.True(contains [1; 2; 3] c)
    Assert.True(contains [1; 3; 2] c)
    Assert.True(contains [2; 1; 3] c)
    Assert.True(contains [2; 3; 1] c)
    Assert.True(contains [3; 1; 2] c)
    Assert.True(contains [3; 2; 1] c)

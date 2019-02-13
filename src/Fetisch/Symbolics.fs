// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.Symbolics

open Fetisch

type Expression =
    | Number of BigRational
    | Variable of string
    | Sum of Expression list
    | Product of Expression list
    | Power of baseExpr: Expression * expExpr: Expression
    with
        member this.Precedence =
            match this with
            | Number _ -> 5
            | Variable _ -> 5
            | Sum _ -> 4
            | Product _ -> 3
            | Power _ -> 2

        override this.ToString() =
            let embrace (e: Expression) =
                "(" + e.ToString() + ")"
                //if this.Precedence > e.Precedence then
                //    e.ToString()
                //else
                //    "(" + e.ToString() + ")"
            match this with
            | Number n -> sprintf "%s" (n.ToString())
            | Variable v -> v
            | Sum s ->
                let folder a t =
                    if a <> "" then
                        sprintf "%s + %s" a (embrace t)
                    else
                        t.ToString()
                List.fold folder "" s
            | Product (p) ->
                let folder a t =
                    if a <> "" then
                        sprintf "%s * %s" a (embrace t)
                    else
                        t.ToString()
                List.fold folder "" p
            | Power (b, e) ->
                sprintf "%s^%s" (embrace b) (embrace e)

// (2 + 3) * 4
// (2 + 3 + 4) * 5

module Operations =
    let zero = Number(BigRational.Zero)
    let one = Number(BigRational.One)
    let variable (name: string) = Variable name
    let number n = Number n

    let add (a: Expression) (b: Expression): Expression =
        match a, b with
        | Sum(s1), Sum(s2) ->
            Sum(s1 @ s2)
        | Sum(s1), _ ->
            Sum(s1 @ [b])
        | _, Sum(s2) ->
            Sum([a] @ s2)
        | _ ->
            Sum([a; b])

    let mul (a: Expression) (b: Expression): Expression =
        match a, b with
        | Product(p1), Product(p2) ->
            Product(p1 @ p2)
        | _ ->
            Product([a; b])

type Expression with
    static member Zero = Number(BigRational.Zero)
    static member One = Number(BigRational.One)

    static member ( + ) (a: Expression, b: Expression): Expression =
        Operations.add a b

    static member ( * ) (a: Expression, b: Expression): Expression =
        Operations.mul a b

module Test =
    open Operations
    let main () =
        let a = number 2N
        let b = number 3N
        let c = number 4N
        let d = (a + b) * c
        printfn "d: %A" d


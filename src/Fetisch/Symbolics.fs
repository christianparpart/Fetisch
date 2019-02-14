// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

// ===========================================================================================
// This namespace/module is an attempt to replace Fetisch.SymbolicAlgebra
// with better simplification in mind, that is, not using binary tree nodes for
// addition and multiplication but tree nodes with N childs for Sum & Product,
// whereas Sum and Product nodes may not be directly connected
// (i.e. Sum/Product composition with itself must be always flattened).
//
// It may be beneficial to also automatically constant-fold and fixiate constants
// at a fixed child-position (say: first) in Sum (last) and Product (first, i.e. coefficient).
// ===========================================================================================

namespace Fetisch.Experimental.Symbolics

open Fetisch
open System

type Constant =
    | E
    | I
    | Pi

type Function =
    | Sin | Cos | Tan
    | Ln | Log | Exp
    | Abs

[<StructuredFormatDisplay("{AsString}")>]
type Expression =
    | Power of Expression * Expression
    | Product of Expression list // Product's must be flattened & first element may be the only one being Number (or Constant)
    | Sum of Expression list     // Sum's must be flattened
    | Function of Function * Expression
    | Variable of string
    | Constant of Constant
    | Number of BigRational
    with
        member this.Precedence =
            match this with
            | Number _ -> 5
            | Constant _ -> 5
            | Variable _ -> 5
            | Function _ -> 5
            | Sum _ -> 4
            | Product _ -> 3
            | Power _ -> 2

        member this.AsString = this.ToString()

        override this.ToString() =
            let embrace (e: Expression) =
                if this.Precedence > e.Precedence
                then sprintf "(%s)" (e.ToString())
                else e.ToString()
            match this with
            | Number n -> sprintf "%s" (n.ToString())
            | Variable v -> v
            | Constant c -> c.ToString()
            | Function (f, e) -> sprintf "%s(%s)" (f.ToString()) (e.ToString())
            | Sum s ->
                let folder a t =
                    match a with
                    | "" -> t.ToString()
                    | _  -> sprintf "%s + %s" a (t.ToString())
                "(" + (List.fold folder "" s) + ")"
            | Product p ->
                let folder a t =
                    match a with
                    | "" -> t.ToString()
                    | _  -> sprintf "%s * %s" a (t.ToString())
                "(" + (List.fold folder "" p) + ")"
            | Power (b, e) ->
                sprintf "%s^%s" (embrace b) (embrace e)

module Operations =
    let zero = Number(BigRational.Zero)
    let one = Number(BigRational.One)
    let minusOne = Number(BigRational.MinusOne)

    let variable (name: string) = Variable name
    let number n = Number n

    //let isZero = function | zero -> true | _ -> false
    //let isOne = function | one -> true | _ -> false

    // Order-relation (<) between two expressions.
    let internal orderRelation (x: Expression) (y: Expression): bool =
        let rec relation a b =
            match a, b with
            | Number x, Number y -> x < y
            | Number _, _ -> true
            | _, Number _ -> false
            | Constant x, Constant y -> x < y
            | Constant _, _ -> true
            | _, Constant _ -> false
            | Variable x, Variable y -> x < y
            | Variable _, _ -> true
            | _, Variable _ -> false
            | Sum xs, Sum ys -> zippedRelation xs ys
            | Sum xs, y -> zippedRelation xs [y]
            | x, Sum ys -> zippedRelation [x] ys
            | Product xs, Product ys -> zippedRelation xs ys
            | Product xs, y -> zippedRelation xs [y]
            | x, Product ys -> zippedRelation [x] ys
            | _ -> false
        and zippedRelation a b =
            match a, b with
            | x::xs, y::ys when x <> y -> relation x y
            | x::xs, y::ys -> zippedRelation xs ys
            | _ -> false
        relation x y

    // Creates the Sum of two summands.
    let add (x: Expression) (y: Expression): Expression =
        let merge ax bx =
            ax @ bx
        match x, y with
        | Sum(ax), Sum(bx) ->
            Sum(merge ax bx)
        | Sum(ax), b ->
            Sum(merge ax [b])
        | a, Sum(bx) ->
            Sum(merge [a] bx)
        | a, b ->
            Sum(merge [a] [b])

    // Creates the product of two factors.
    let mul (a: Expression) (b: Expression): Expression =
        let merge ax bx =
            ax @ bx
        match a, b with
        | Product(ax), Product(bx) ->
            Product(merge ax bx)
        | Product(ax), b ->
            Product(merge ax [b])
        | a, Product(bx) ->
            Product(merge [a] bx)
        | _ ->
            Product(merge [a] [b])

    // Constructs additive inverse.
    let negate (x: Expression) = mul minusOne x

    // Constructs multiplicative inverse.
    let invert = function
        | Number x -> Number(1N / x)
        | x -> Power(x, minusOne)

    let sub (x: Expression) (y: Expression): Expression =
        add x (negate y)

    let div (x: Expression) (y: Expression) =
        mul x (invert y)

type Expression with
    static member Zero = Number(BigRational.Zero)
    static member One = Number(BigRational.One)

    static member FromInt (x: int) = Number(BigRational.FromInt x)
    static member FromBigInt (x: bigint) = Number(BigRational.FromBigInt x)

    static member ( + ) (a: Expression, b: Expression) = Operations.add a b
    static member ( - ) (a: Expression, b: Expression) = Operations.sub a b
    static member ( * ) (a: Expression, b: Expression) = Operations.mul a b
    static member ( / ) (a: Expression, b: Expression) = Operations.div a b

    static member Pow (a: Expression, b: Expression) = Power(a, b)
    static member Abs (a: Expression) = Function(Function.Abs, a)

[<RequireQualifiedAccess>]
module NumericLiteralG =
    let FromZero() = Expression.Zero
    let FromOne() = Expression.One
    let FromInt32 (x: int32) = x |> Expression.FromInt
    let FromInt64 (x: int64) = Expression.FromBigInt (bigint x)
    let FromString (x: string) = bigint.Parse x |> Expression.FromBigInt

module Test =
    let main () =
        let d = ((2G + 3G) * 4G) ** 5G / 6G
        printfn "d: %A" d


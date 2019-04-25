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
                let folder (a: string) (t: Expression) : string =
                    if a = "" then embrace t else sprintf "%s + %s" a (embrace t)
                List.fold folder "" s
            | Product p ->
                let folder (a: string) (t: Expression) : string =
                    if a = "" then embrace t else sprintf "%s * %s" a (embrace t)
                List.fold folder "" p
            | Power (b, e) ->
                sprintf "%s^%s" (embrace b) (embrace e)

module Patterns =
    open FSharp.Core.LanguagePrimitives

    let (|Zero|_|) = function
        | Number n when n.IsZero -> Some Zero
        | _ -> None

    let (|One|_|) = function
        | Number n when n.IsOne -> Some One
        | _ -> None

    let (|Positive|_|) = function
        | Number n when n > GenericZero -> Some Positive
        | Constant E | Constant Pi -> Some Positive
        | _ -> None

    let (|Negative|_|) = function
        | Number n when n < GenericZero -> Some Negative
        | _ -> None

    let (|Terminal|_|) = function
        | Number _
        | Variable _
        | Constant _ as t ->
            Some t
        | _ ->
            None

module Operations =
    open Patterns
    open FSharp.Core.LanguagePrimitives

    let zero = Number(BigRational.Zero)
    let one = Number(BigRational.One)
    let minusOne = Number(BigRational.MinusOne)

    let number n = Number n
    let variable (name: string) = Variable name
    let var = variable

    let isZero = function | Zero -> true | _ -> false
    let isOne  = function | One  -> true | _ -> false

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
            | Function (fx, x), Function (fy, y) -> if fx <> fy then fx < fy else relation x y
            | Power (xr, xp), y -> if xr <> y then relation xr y else relation xp one
            | x, Power (yr, yp) -> if x <> yr then relation x yr else relation one yp
            | Product xs, Product ys -> zippedRelation xs ys
            | Product xs, y -> zippedRelation xs [y]
            | x, Product ys -> zippedRelation [x] ys
            | Sum xs, Sum ys -> zippedRelation xs ys
            | Sum xs, y -> zippedRelation xs [y]
            | x, Sum ys -> zippedRelation [x] ys
        and zippedRelation a b =
            match a, b with
            | x::xs, y::ys when x <> y -> relation x y
            | x::xs, y::ys -> zippedRelation xs ys
            | [], y::ys -> true
            | _, [] -> false
        relation x y

    // Creates the Sum of two summands.
    let rec add (x: Expression) (y: Expression): Expression =
        let sum = function
            | Number a, Number b -> Number(a + b)
            | a, b -> Sum([a; b])

        //  Matches a multiplicative term in the form of (a*x)
        let (|Term|_|) = function
            | Number _ -> None
            | Product [a; b] ->
                printfn "|Term| %A * %A" a b
                Some (a, b)
            | Product (a::rest) -> Some (a, Product rest)
            | x ->
                printfn "|Term| one * (%A)" x
                Some (one, x)

        // Folds 2 expression lists into a single expression list.
        let rec fold = function
            | Zero::acc, a, b -> fold (acc, a, b)
            | Term(a, x)::p, Term(b, x')::q, r when x = x' -> fold ((mul (sum (a, b)) x)::p, q, r)
            | Term(a, x)::p, r, Term(b, x')::q when x = x' -> fold ((mul (sum (a, b)) x)::p, q, r)
            | r, Term(a, x)::p, Term(b, x')::q when x = x' -> fold ((mul (sum (a, b)) x)::r, p, q)
            | r, x::xs, y::ys when orderRelation x y       -> fold (x::r, xs, y::ys)
            | r, x::xs, y::ys                              -> fold (y::r, x::xs, ys)
            | r, x::xs, []                                 -> fold (x::r, xs, [])
            | r, [], y::ys                                 -> fold (y::r, [], ys)
            | r, [], []                                    -> r

        let merge (u: Expression list) (v: Expression list): Expression =
            match fold ([], u, v) with
            | []  -> zero
            | [x] -> x
            | x   -> Sum (List.rev x)

        let rec valueAdd (v: BigRational) (x: Expression) : Expression =
            match x with
            | Sum [] -> Number v
            | Sum ((Number a)::ax) -> valueAdd (v + a) (Sum ax)
            | Sum ax -> if v = GenericZero then x else Sum ((Number v)::ax)
            | x when v = GenericZero -> x
            | _ -> Sum [Number v; x]

        match (x, y) with
        | Zero, a -> a
        | a, Zero -> a
        // one constant value and one something else
        | Number a, b -> valueAdd a b
        | a, Number b -> valueAdd b a
        // both Sum have constant value
        | Sum ((Number a)::ax), Sum ((Number b)::bx) -> valueAdd (a + b) (merge ax bx)
        // one Sum has constant value, other is Sum without constant value
        | Sum ((Number a)::ax), Sum bx -> valueAdd a (merge ax bx)
        | Sum ax, Sum ((Number b)::bx) -> valueAdd b (merge ax bx)
        // one Sum has constant value, other is not a Sum
        | Sum ((Number a)::ax), b -> valueAdd a (merge ax [b])
        | a, Sum ((Number b)::bx) -> valueAdd b (merge bx [a])
        // two arbitrary Sum lists
        | Sum ax, Sum bx -> merge ax bx
        // one Sum and something else
        | Sum ax, b -> merge ax [b]
        | a, Sum bx -> merge [a] bx
        // two non-Sum summands
        | a, b -> merge [a] [b]

    // Creates the product of two factors.
    and mul (x: Expression) (y: Expression): Expression =
        let (|Term|_|) = function
            | Number _ -> None
            | Power(a, b) -> Some (a, b)
            | x -> Some (x, one)

        let rec fold = function
            | One::r, a, b -> fold (r, a, b)
            | Term(ab,ae)::r, Term(xb, xe)::xs, y when ab = xb -> fold ((pow ab (add ae xe))::r, xs, y)
            | Term(ab,ae)::r, y, Term(xb, xe)::xs when ab = xb -> fold ((pow ab (add ae xe))::r, xs, y)
            | r, Term(xb,xe)::xs, Term(yb,ye)::ys when xb = yb -> fold ((pow xb (add xe ye))::r, xs, ys)
            | r, x::xs, y::ys -> if orderRelation x y then fold (x::r, xs, y::ys) else fold (y::r, x::xs, ys)
            | r, x::xs, y -> fold (x::r, xs, y)
            | r, [], y::ys -> fold (y::r, ys, [])
            | r, [], [] -> r

        let merge (u: Expression list) (v: Expression list): Expression =
            match fold ([], u, v) with
            | []  -> one
            | [t] -> t
            | s   -> Product(List.rev s)

        match x, y with
        | One, a | a, One -> a
        | Zero, _ | _, Zero -> zero
        | Number a, Number b -> Number (a * b)
        | Product (Number(a)::ax), Product (Number(b)::bx) -> mul (Number(a * b)) (merge ax bx)
        | Product(ax), Product(bx) -> merge ax bx
        | Product(ax), b -> merge ax [b]
        | a, Product(bx) -> merge [a] bx
        | a, b -> merge [a] [b]

    and pow (x: Expression) (y: Expression) =
        match x, y with
        | _, Zero -> one
        | _, One -> x
        | Number ax, Number bx ->
            Number(ax ** bx)
        | Product ax, Number b when b.IsInteger ->
            Product(ax |> List.map (fun z -> pow z y))
        | Power (r, p), Number b when b.IsInteger -> pow r (mul p y)
        | _, _ -> Power(x, y)

    // Constructs additive inverse.
    let negate (x: Expression) = mul minusOne x

    let sub (x: Expression) (y: Expression): Expression =
        add x (negate y)

    // Constructs multiplicative inverse.
    let invert = function
        | Number x -> Number(1N / x)
        | x -> Power(x, minusOne)

    let div (x: Expression) (y: Expression) =
        mul x (invert y)

type Expression with
    static member Zero = Operations.zero
    static member One = Operations.one

    static member FromInt (x: int) = Number(BigRational.FromInt x)
    static member FromBigInt (x: bigint) = Number(BigRational.FromBigInt x)

    static member ( + ) (a: Expression, b: Expression) = Operations.add a b
    static member ( - ) (a: Expression, b: Expression) = Operations.sub a b
    static member ( * ) (a: Expression, b: Expression) = Operations.mul a b
    static member ( / ) (a: Expression, b: Expression) = Operations.div a b

    static member ( ~- ) (a: Expression) = Operations.sub Operations.zero a
    static member ( ~+ ) (a: Expression) = a

    static member Pow (a: Expression, b: Expression) = Operations.pow a b
    static member Abs (a: Expression) = Function(Function.Abs, a)

[<RequireQualifiedAccess>]
module NumericLiteralG =
    let FromZero() = Operations.zero
    let FromOne() = Operations.one
    let FromInt32 (x: int32) = x |> Expression.FromInt
    let FromInt64 (x: int64) = Expression.FromBigInt (bigint x)
    let FromString (x: string) = bigint.Parse x |> Expression.FromBigInt

module Parser =
    type Token =
        | Illegal
        | Whitespace
        | Eof
        | Plus
        | Minus
        | Mul
        | Div
        | Pow
        | NumberLiteral
        | Identifier
        | RndOpen
        | RndClose

    type TokenInfo = { Token: Token; Literal: string }

    //let tokenize (s: string) : seq<TokenInfo> =
    //    let str = Seq.toList s
    //    let rec tokenize (str: char list) : TokenInfo * (char list) =
    //        match str with
    //        | ' '::xs -> { Token = Whitespace; Literal = " " }, xs
    //        | [] -> { Token = Eof; Literal = "" }, []
    //        | unknown::xs -> { Token = Illegal; Literal = string unknown }, xs
    //    seq {
    //        let t, r = tokenize (Seq.toList s)
    //        yield t
    //        yield! tokenize r
    //    }

    // let parseString (s: string) : Expression =

module Test =
    open Operations
    let main () =
        let x = var "x"
        printfn "x: %A" (3G + (x + 5G))


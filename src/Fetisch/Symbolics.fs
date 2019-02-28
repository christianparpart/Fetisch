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
        printfn "Add: x=%A, y=%A" x y
        let (|Term|_|) = function
            | Number _ -> None
            | Product [a; b] -> Some (a, b)
            | Product (a :: rest) -> Some (a, Product rest)
            | x -> Some (one, x)
        let sum (a: Expression, b: Expression): Expression =
            printfn "Add.Sum: a=%A, b=%A" a b
            match a, b with
            | Number a', Number b' -> Number(a' + b')
            | _ -> Sum([a; b])
        let merge (u: Expression list) (v: Expression list): Expression =
            printfn "Add.Merge: u=%A, v=%A" u v
            let rec fold (acc: Expression list) (u: Expression list) (v: Expression list): Expression list =
                printfn "Add.Merge.Fold: acc=%A, u=%A, v=%A" acc u v
                match acc, u, v with
                | Zero::acc', _, _ ->
                    printfn "  match (Zero::acc, _, _): acc'=%A" acc'
                    fold acc' u v
                | Term(ac, at)::acc', Term(xc, xt)::xs, y when at = xt ->
                    printfn "  match ac: %A; xc: %A" ac xc
                    fold ((mul (sum (ac, xc)) at)::acc') xs y
                | Term(ac, at)::acc', x, Term(yc, yt)::ys when at = yt ->
                    printfn "  match ac: %A; yc: %A" ac yc
                    fold ((mul (sum (ac, yc)) at)::acc') x ys
                | _, Term(xc, xt)::xs, Term(yc, yt)::ys when xt = yt ->
                    // (a*x + p) + (b*y + q) = (a+b)*x + p + q
                    printfn "match (_, xc: %A; yc: %A)" xc yc
                    fold ((mul (sum (xc, yc)) xt)::acc) xs ys
                | _, x::xs, y::ys ->
                    printfn "  match (_, x::xs, y::ys): x=%A, xs=%A, y=%A, ys=%A" x xs y ys
                    if orderRelation x y
                    then fold (x::acc) xs v
                    else fold (y::acc) u ys
                | _, x::xs, [] ->
                    printfn "  match (_, x::xs, []): x=%A, xs=%A" x xs
                    fold (x::acc) xs []
                | _, [], y::ys ->
                    printfn "  match (_, [], y::ys): y=%A, ys=%A" y ys
                    fold (y::acc) [] ys
                | _, [], [] ->
                    printfn "  match (_, [], [])"
                    acc
            match fold [] u v with
            | []  -> zero
            | [x] -> x
            | x   -> Sum (List.rev x)
        match x, y with
        | Zero, _ -> y
        | _, Zero -> x
        | Number a, Number b ->
            printfn "Add.match (num, num): a=%A, b=%A" a b
            Number (a + b)
        | Sum (Number(a)::ax), Sum (Number(b)::bx) ->
            printfn "Add.match (Sum(Num(a)::ax), Sum(Num(b)::bx): a=%A, ax=%A, b=%A, bx=%A" a ax b bx
            add (Number(a + b)) (merge ax bx)
        | Sum (Number(a)::ax), Number b ->
            printfn "Add.match (Sum(Num(a)::ax) Num(b): a=%A, ax=%A, b=%A" a ax b
            add (Number (a + b)) (Sum ax)
        | Sum(ax), Sum(bx) ->
            printfn "Add.match (Sum(ax), Sum(bx)): ax=%A, bx=%A" ax bx
            merge ax bx
        | Sum(ax), b ->
            printfn "Add.match (Sum(ax), b): ax=%A, b=%A" ax b
            merge ax [b]
        | a, Sum(bx) ->
            printfn "Add.match (a, Sum(bx)): a=%A, bx=%A" a bx
            merge [a] bx
        | a, b ->
            printfn "Add.match (a, b): a=%A, b=%A" a b
            merge [a] [b]

    // Creates the product of two factors.
    and mul (x: Expression) (y: Expression): Expression =
        let (|Term|_|) = function
            | Number _ -> None
            | Power(a, b) -> Some (a, b)
            | any -> Some (any, one)

        let merge (u: Expression list) (v: Expression list): Expression =
            let rec fold (acc: Expression list) (u: Expression list) (v: Expression list): Expression list =
                match acc, u, v with
                | One::acc', _, _ ->
                    fold acc' u v
                | Term(ab,ae)::acc', Term(xb,xe)::xs, y
                | Term(ab,ae)::acc', y, Term(xb,xe)::xs when ab = xb ->
                    fold ((pow ab (add ae xe))::acc') xs y
                | _, Term(xb,xe)::xs, Term(yb,ye)::ys when xb = yb ->
                    fold ((pow xb (add xe ye))::acc) xs ys
                | _, x::xs, y::ys ->
                    if orderRelation x y
                    then fold (x::acc) xs v
                    else fold (y::acc) u ys
                | _, x::xs, y -> fold (x::acc) xs y
                | _, [], y::ys -> fold (y::acc) ys []
                | _, [], [] -> acc
            match fold [] u v with
            | []  -> one
            | [t] -> t
            | s   -> Product(List.rev s)

        match x, y with
        | One, a | a, One -> a
        | Zero, _ | _, Zero -> zero
        | Number a, Number b ->
            Number (a * b)
        | Product (Number(a)::ax), Product (Number(b)::bx) ->
            mul (Number(a * b)) (merge ax bx)
        | Product(ax), Product(bx) ->
            merge ax bx
        | Product(ax), _ ->
            merge ax [y]
        | _, Product(bx) ->
            merge [x] bx
        | _ ->
            merge [x] [y]

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

module Test =
    open Operations
    let main () =
        let x = var "x"
        //let d = (x * (2G + 3G) * 4G) ** 2G
        //let d = x * (x ** (3G + x))
        //let d = 1G + (3G + x)
        //let d = (2G + x) + 1G
        let d = (2G + 3G) + 4G
        printfn "d: %A" d


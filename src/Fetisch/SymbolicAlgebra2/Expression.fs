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
        /// Retrieves precedence of this expression. Higher number means higher precedence to other expressions.
        member this.Precedence =
            match this with
            | Number _ -> 5
            | Constant _ -> 5
            | Variable _ -> 5
            | Function _ -> 4
            | Sum _ -> 1
            | Product _ -> 2
            | Power _ -> 3

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

        // Used for StructuredFormatDisplay for easy information access in IDE debugging.
        member this.AsString =
            let typeName =
                match this with
                | Power (_, _) -> "Power"
                | Product _ -> "Product"
                | Sum _ -> "Sum"
                | Function (_, _) -> "Function"
                | Variable _ -> "Variable"
                | Constant _ -> "Constant"
                | Number _ -> "Number"
            (sprintf "%s: %s" typeName (this.ToString()))

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

    //  Matches a multiplicative term in the form of (a*x)
    let (|IsProduct|_|) = function
        | Number _ -> None
        | Product [a; b] -> Some (a, b)
        | Product (a::rest) -> Some (a, Product rest)
        | x -> Some (Number(BigRational.One), x)

    // Matches a term in form of base^exponent.
    let (|IsPower|_|) = function
        | Number _ -> None
        | Power(a, b) -> Some (a, b)
        | x -> Some (x, Number(BigRational.One))

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

    let rec internal zipped (f: Expression -> Expression -> int) (a: Expression list) (b: Expression list): int =
        match a, b with
        | x::xs, y::ys when x <> y -> f x y
        | x::xs, y::ys -> zipped f xs ys
        | [], y::ys -> -1  // everything is equal up to the length of the left list
        | _, [] -> +1      // everything is equal up to the length of the right list

    // Returns -1 (<), 0 (=), or +1 (>)
    let compare (x: Expression) (y: Expression): int =
        let cmp a b =
            if a < b then -1
            elif a = b then 0
            else +1

        let rec compare a b =
            match a, b with
            | Number x, Number y -> cmp x y
            | Constant x, Constant y -> cmp x y
            | Variable x, Variable y -> cmp x y
            | Sum xs, Sum ys -> zippedCompare xs ys
            | Product xs, Product ys -> zippedCompare xs ys
            | Power (xr,xp), Power (yr,yp) -> if xr <> yr then compare xr yr else compare xp yp
            | Function (fx, x), Function (fy, y) -> if fx <> fy then cmp fx fy else compare x y
            // lose type matches
            | Number _, _ -> -1
            | _, Number _ -> +1
            | Constant _, _ -> -1
            | _, Constant _ -> +1
            | Variable _, _ -> -1
            | _, Variable _ -> +1
            | Sum xs, y -> zippedCompare xs [y]
            | x, Sum ys -> zippedCompare [x] ys
            | Product xs, y -> zippedCompare xs [y]
            | x, Product ys -> zippedCompare [x] ys
            | Power (xr, xp), y -> if xr <> y then compare xr y else compare xp one
            | x, Power (yr, yp) -> if x <> yr then compare x yr else compare one yp
        and zippedCompare a b = zipped compare a b
        compare x y

    // Order-relation (<) between two expressions.
    let orderRelation (x: Expression) (y: Expression): bool =
        (compare x y) < 0

    // Creates the Sum of two summands.
    let rec add (x: Expression) (y: Expression): Expression =
        let sum = function
            | Number a, Number b -> Number(a + b)
            | a, b -> Sum([a; b])

        // Folds 2 expression lists into a single expression list.
        let rec fold = function
            | Zero::acc, a, b ->
                fold (acc, a, b)
            | IsProduct(a, x)::p, IsProduct(b, x')::q, r when x = x' ->
                fold ((mul (sum (a, b)) x)::p, q, r)
            | IsProduct(a, x)::p, r, IsProduct(b, x')::q when x = x' ->
                fold ((mul (sum (a, b)) x)::p, q, r)
            | r, IsProduct(a, x)::p, IsProduct(b, x')::q when x = x' ->
                fold ((mul (sum (a, b)) x)::r, p, q)
            | r, x::xs, y::ys when orderRelation x y ->
                fold (x::r, xs, y::ys)
            | r, x::xs, y::ys ->
                fold (y::r, x::xs, ys)
            | r, x::xs, [] ->
                fold (x::r, xs, [])
            | r, [], y::ys ->
                fold (y::r, [], ys)
            | r, [], [] ->
                r

        let merge (u: Expression list) (v: Expression list): Expression =
            match fold ([], u, v) with
            | []  -> zero
            | [x] -> x
            | x   -> Sum (List.rev x)

        // Adds number `v` to expression `x`
        let rec valueAdd (v: BigRational) (x: Expression) : Expression =
            match x with
            | Number a -> Sum [Number (v + a)]
            | Sum [] -> Number v
            | Sum [Number a] -> Sum [Number (v + a)]
            | Sum ((Number a)::ax) -> valueAdd (v + a) (Sum ax)
            | Sum ax -> if v = GenericZero then x else Sum ((Number v)::ax)
            | _ -> if v = GenericZero then x else Sum [Number v; x]

        match (x, y) with
        | Zero, a -> a
        | a, Zero -> a
        // one constant value and one something else
        | Number a, Number b -> Number (a + b)
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
        //  Folds 2 expression lists into a single expression list.
        let rec fold (result, lhs, rhs) =
            // a * a^(-1) = 1
            match (result, lhs, rhs) with
            | One::r, a, b ->
                fold (r, a, b)
            | IsPower(ab, ae)::r, IsPower(xb, xe)::xs, y when ab = xb && xe = Number(-1N) ->
                // a^b * r * a^(-1) * c * y = r * c * y
                fold (r, xs, y)
            | IsPower(ab, ae)::r, IsPower(xb, xe)::xs, y when ab = xb ->
                // a^a' * a^x' * y -> a^(a' + x') * y
                fold ((pow ab (add ae xe))::r, xs, y)
            | IsPower(ab, ae)::r, y, IsPower(xb, xe)::xs when ab = xb ->
                fold ((pow ab (add ae xe))::r, xs, y)
            | r, IsPower(xb,xe)::xs, IsPower(yb,ye)::ys when xb = yb ->
                fold ((pow xb (add xe ye))::r, xs, ys)
            | r, x::xs, y::ys -> if orderRelation x y then fold (x::r, xs, y::ys) else fold (y::r, x::xs, ys)
            | r, x::xs, y -> fold (x::r, xs, y)
            | r, [], y::ys -> fold (y::r, ys, [])
            | r, [], [] -> r

        let merge (u: Expression list) (v: Expression list): Expression =
            match fold ([], u, v) with
            | []  -> one
            | [t] -> t
            | s   -> Product(List.rev s)

        // Multiplies number `v` to expression x
        let rec valueMul (v: BigRational) (x: Expression) =
            match x with
            | Number a -> Product [Number (v * a)]
            | Product [] -> Number v
            | Product [Number a] -> Product [Number (v * a)]
            | Product ((Number a)::ax) -> valueMul (v * a) (Product ax)
            | Product ax -> if v = GenericOne then x else Product ((Number v)::ax)
            | _ -> if v = GenericOne then x else Product [Number v; x]

        match x, y with
        | One, a -> a
        | a, One -> a
        | Zero, _ -> zero
        | _, Zero -> zero
        | Number a, Number b ->
            Number (a * b)
        | Number a, b ->
            valueMul a b
        | a, Number b ->
            valueMul b a
        | Product (Number(a)::ax), Product (Number(b)::bx) ->
            mul (Number(a * b)) (merge ax bx)
        | Product(ax), Product(bx) ->
            merge ax bx
        | Product(ax), b ->
            merge ax [b]
        | a, Product(bx) ->
            merge [a] bx
        | a, b ->
            merge [a] [b]

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

//module Parser =
//    let parseString (s: string) : Expression =

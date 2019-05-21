// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.SymbolicAlgebra

open System

[<StructuredFormatDisplay("{AsString}")>]
type Expr =
    | AddExpr of left : Expr * right : Expr
    | SubExpr of left : Expr * right : Expr
    | NegExpr of subExpr : Expr
    | AbsExpr of subExpr : Expr
    | MulExpr of left : Expr * right : Expr
    | DivExpr of left : Expr * right : Expr
    | PowExpr of left : Expr * right : Expr
    | Variable of symbol : string
    | Number of literal : bigint
    with
        static member Zero = Number(0I)
        static member One = Number(1I)
        static member FromInt (x: int) = Number(bigint x)
        static member FromBigInt (x: bigint) = Number(x)

        static member ( + ) (a: Expr, b: Expr) : Expr = AddExpr(a, b)
        static member ( - ) (a: Expr, b: Expr) : Expr = SubExpr(a, b)
        static member ( * ) (a: Expr, b: Expr) : Expr = MulExpr(a, b)
        static member ( / ) (a: Expr, b: Expr) : Expr = DivExpr(a, b)
        static member ( ^^ ) (a: Expr, b: Expr) : Expr = PowExpr(a, b)
        static member ( ~- ) (x: Expr) : Expr = NegExpr(x)
        static member ( ~+ ) (x: Expr) : Expr = x
        static member Abs (x: Expr) : Expr = AbsExpr(x)

        member this.Precedence =
            match this with
                | AddExpr(_, _) -> 1
                | SubExpr(_, _) -> 1
                | AbsExpr(_)    -> 1
                | NegExpr(_)    -> 0 // ?
                | MulExpr(_, _) -> 2
                | DivExpr(_, _) -> 2
                | PowExpr(_, _) -> 3
                | Variable(_) -> 4
                | Number(_) -> 4

        member this.AsString = this.ToString()

        override this.ToString () : string =
            let embrace (e: Expr) =
                if e.Precedence <= this.Precedence then
                    sprintf "(%s)" (e.ToString())
                else
                    e.ToString()
            match this with
            | AddExpr(left, right) -> sprintf "%s + %s" (embrace left) (embrace right)
            | SubExpr(left, right) -> sprintf "%s - %s" (embrace left) (embrace right)
            | AbsExpr(subExpr)     -> sprintf "|%s|" (embrace subExpr)
            | NegExpr(subExpr)     -> sprintf "-%s" (embrace subExpr)
            | MulExpr(left, right) -> sprintf "%s * %s" (embrace left) (embrace right)
            | DivExpr(left, right) -> sprintf "%s / %s" (embrace left) (embrace right)
            | PowExpr(left, right) -> sprintf "%s^%s" (embrace left) (embrace right)
            | Variable(symbol) -> sprintf "%s" symbol
            | Number(value) -> sprintf "%s" (value.ToString())

// Provides user defined literal with suffix G.
[<RequireQualifiedAccess>]
module NumericLiteralG =
    let FromZero() = Expr.Zero
    let FromOne() = Expr.One
    let FromInt32 (x: int32) = x |> Expr.FromInt
    let FromInt64 (x: int64) = Expr.FromBigInt (bigint x)
    let FromString (x: string) = bigint.Parse x |> Expr.FromBigInt

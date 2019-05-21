// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.SymbolicAlgebra

open System
open Core.LanguagePrimitives

module Calculator =
    type SymbolMap = Map<string, Expr>

    let calculate (smap: SymbolMap) (e: Expr) =
        let rec calculate e =
            match e with
            | AddExpr(a, b) -> (calculate a) + (calculate b)
            | SubExpr(a, b) -> (calculate a) - (calculate b)
            | AbsExpr(sexp) -> abs (calculate sexp)
            | NegExpr(sexp) -> -(calculate sexp)
            | MulExpr(a, b) -> (calculate a) * (calculate b)
            | DivExpr(a, b) -> (calculate a) / (calculate b)
            | PowExpr(a, b) ->
                let a' = calculate a
                let b' = int (calculate b)
                bigint.Pow(a', b')
            | Variable(s) ->
                match smap.TryFind s with
                | Some v -> calculate v
                | None -> invalidOp (sprintf "Unknown symbol `%s`." s)
            | Number(n) -> n
        calculate e

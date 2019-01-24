// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.SymbolicAlgebra

open Core.LanguagePrimitives

// TODO
// - ensure numbers are always on the left side of a term
// - ensure number-only-terms are on the most-right
// - (C1 * a) * C2 = (C1 * C2) * a

module Simplifier =
    let standardRules =
        Transform.createRules [
            // additive
            ("a+0", "a")
            ("a+a", "2*a")
            ("a-a", "0")
            ("0-a", "-a")
            ("-(-a)", "a")

            // multiplicative
            ("0*a", "0")
            ("1*a", "a")
            ("a/a", "1")
            ("a/1", "a")
            ("a*a", "a^2")
            ("-1*a", "-a")
            ("(-a)*b", "-(a*b)")

            // distributivity
            ("a*b + a*c", "a*(b+c)")
            ("a*c + b*c", "(a+b)*c")

            // exp
            ("a^0", "1")
            ("a^1", "a")
            ("a*a^b", "a^(b+1)")
            ("a^c*b^b", "(a*b)^b")
            ("(a^b)^c", "a^(b*c)")

            // more complex
            ("(a+b) * (a-b)", "a^2 - b^2")
            ("a^2 + 2*a*b + b^2", "(a+b)^2")
        ]

    // Folds constant operations.
    // Also orders operands to a canonical schemes, such as: scalars to the left, fixed sums on the right.
    let rec constantFold (expr: Expr): Expr =
        match expr with
        | NegExpr(a) ->
            match constantFold a with
            | NumberExpr a' -> NumberExpr(-a')
            | NegExpr(a') -> constantFold a'
            | a' -> NegExpr(a')
        | AbsExpr(a) ->
            match constantFold a with
            | NumberExpr a' -> NumberExpr(abs a')
            | a' -> AbsExpr(a')
        | AddExpr(a, b) ->
            match constantFold a, constantFold b with
            | NumberExpr a', NumberExpr b' ->
                NumberExpr(a' + b')
            // Force fixed sums to the right.
            | NumberExpr a', b' ->
                AddExpr(b', NumberExpr(a'))
            | a', b' ->
                AddExpr(a', b')
        | SubExpr(a, b) ->
            match constantFold a, constantFold b with
            | NumberExpr a', NumberExpr b' -> NumberExpr(a' - b')
            | a', b' -> SubExpr(a', b')
        | MulExpr(a, b) ->
            match constantFold a, constantFold b with
            | NumberExpr a', NumberExpr b' -> NumberExpr(a' * b')
            // force move coefficients to the left side
            | a', NumberExpr b' -> MulExpr(NumberExpr(b'), a')
            | a', b' -> MulExpr(a', b')
        | DivExpr(a, b) ->
            match constantFold a, constantFold b with
            | NumberExpr a', NumberExpr b' -> NumberExpr(a' / b')
            | a', NumberExpr b' -> DivExpr(NumberExpr(b'), a')
            | a', b' -> DivExpr(a', b')
        | PowExpr(a, b) ->
            match constantFold a, constantFold b with
            | NumberExpr a', NumberExpr b' -> NumberExpr(bigint.Pow(a', int b'))
            | a', b' -> PowExpr(a', b')
        | NumberExpr(_) ->
            expr
        | SymbolExpr(_) ->
            expr

    let simplifyWithRules (expr: Expr) (rules: Transform.Rule list): Expr =
        // Depth-first traversal, optimizing from bottom to top, restart as long as subtree mutates.
        let rec simplify (expr: Expr) =
            let depthFirst (expr: Expr) =
                match expr with
                | NegExpr(a) -> NegExpr(simplify a)
                | AbsExpr(a) -> AbsExpr(simplify a)
                | AddExpr(a, b) -> (AddExpr(simplify a, simplify b))
                | SubExpr(a, b) -> (SubExpr(simplify a, simplify b))
                | MulExpr(a, b) -> (MulExpr(simplify a, simplify b))
                | DivExpr(a, b) -> (DivExpr(simplify a, simplify b))
                | PowExpr(a, b) -> (PowExpr(simplify a, simplify b))
                | SymbolExpr(_) -> expr
                | NumberExpr(_) -> expr
            let applyRules (expr: Expr): Expr =
                match Transform.matchRuleListCommutative expr rules with
                | Some (rule, matches) ->
                    Transform.applyMatches (rule.Transform) matches
                | None ->
                    expr
            let restartOnChange (oldExpr: Expr) (newExpr: Expr): Expr =
                match oldExpr = newExpr with
                | true -> oldExpr
                | false -> simplify newExpr
            // now, trigger the pipeline, *BaM!*
            expr
            |> depthFirst
            |> constantFold
            |> applyRules
            |> restartOnChange expr

        simplify expr

    let simplify (expr: Expr): Expr =
        simplifyWithRules expr standardRules

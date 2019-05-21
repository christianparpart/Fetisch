// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.SymbolicAlgebra

module ExprMatcher =
    // Represents a successful match of symbol to sub-expression.
    type Match = {
        Name: string            // The symbol in the pattern expression.
        Expression: Expr        // The match expression.
    }

    // Tries to merge two results together, making sure that match expressions
    // with the same symbol must match.
    //
    // Returns Some(Match list) with the all symbol+expression tuples being matched.
    // Returns None otherwise (i.e. a conflict between the two was found).
    let private tryMergeMatchLists (a: Match list option) (b: Match list option) : Match list option =
        let rec tryMerge (acc: Match list) (a: Match list) : Match list option =
            // merge a into b and return that
            match a with
            | aHead :: aTail ->
                match List.tryFind (fun (accMatch: Match) -> aHead.Name = accMatch.Name) acc with
                | Some bMatch when aHead.Expression = bMatch.Expression ->
                    tryMerge acc aTail
                | Some _ ->
                    None
                | _ ->
                    // aHead not found in b, hence there can be no conflict
                    tryMerge (aHead :: acc) aTail
            | [] ->
                // first list is empty, hence b contains no match conflicts with a, hence b is valid.
                Some acc
        if a.IsSome && b.IsSome then
            let x = tryMerge [] (a.Value)
            let y = tryMerge (x.Value) (b.Value)
            y
        else
            None

    // Traverse through pattern tree, and associate each variable with a node on "that" expr.
    // respect commutativity
    let rec matchRule (expr: Expr) (pattern: Expr): Match list option =
        match pattern, expr with
        | NegExpr(a), NegExpr(a') ->
            matchRule a' a
        | AbsExpr(a), AbsExpr(a') ->
            matchRule a' a
        | AddExpr(a, b), AddExpr(a', b') ->
            tryMergeMatchLists (matchRule a' a) (matchRule b' b)
        | SubExpr(a, b), SubExpr(a', b') ->
            tryMergeMatchLists (matchRule a' a) (matchRule b' b)
        | MulExpr(a, b), MulExpr(a', b') ->
            tryMergeMatchLists (matchRule a' a) (matchRule b' b)
        | DivExpr(a, b), DivExpr(a', b') ->
            tryMergeMatchLists (matchRule a' a) (matchRule b' b)
        | PowExpr(a, b), PowExpr(a', b') ->
            tryMergeMatchLists (matchRule a' a) (matchRule b' b)
        | Variable(s), _ ->
            Some [{Name = s; Expression = expr}]
        | Number(n), Number(n') when n = n' ->
            Some []
        | _-> None

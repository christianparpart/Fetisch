// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.SymbolicAlgebra

module Transform =
    open ExprMatcher

    type Rule = {
        Pattern: Expr
        Transform: Expr
    }

    // Creates a list of rules by parsing the input list with a string-tuple of (pattern, transform) rules.
    let createRules (rules: (string * string) list): Rule list =
        let foldRule (acc: Rule list) (patternText: string, transformText: string): Rule list =
            let pattern = ExprParser.parseString patternText
            let transform = ExprParser.parseString transformText
            {Pattern = pattern; Transform = transform} :: acc
        List.fold foldRule [] rules

    // Matches a given expression against a list of rules.
    // Returns None if no rule pattern matched, or the rule that matched along with a list of captured sub-expression matches.
    let rec matchRuleList (expr: Expr) (patterns: Rule list): (Rule * (Match list)) option =
        match patterns with
        | pattern :: otherPatterns ->
            match ExprMatcher.matchRule expr (pattern.Pattern) with
            | Some matches ->
                Some (pattern, matches)
            | None ->
                matchRuleList expr otherPatterns
        | [] ->
            None

    // Matches a given expression against a list of rules, respecting commutativity with respect to addition and multiplication.
    let matchRuleListCommutative (expr: Expr) (patterns: Rule list): (Rule * (Match list)) option =
        match matchRuleList expr patterns with
        | Some (rule, matches) ->
            Some (rule, matches)
        | None ->
            match expr with
            | AddExpr(a, b) -> matchRuleList (AddExpr(b, a)) patterns
            | MulExpr(a, b) -> matchRuleList (MulExpr(b, a)) patterns
            | _ -> None

    // Creates a new expression by applying all matches on the expression.
    let rec applyMatches (expr: Expr) (matches: Match list): Expr =
        match expr with
        | AbsExpr(a) -> AbsExpr(applyMatches a matches)
        | NegExpr(a) -> NegExpr(applyMatches a matches)
        | AddExpr(a, b) -> AddExpr(applyMatches a matches, applyMatches b matches)
        | SubExpr(a, b) -> SubExpr(applyMatches a matches, applyMatches b matches)
        | MulExpr(a, b) -> MulExpr(applyMatches a matches, applyMatches b matches)
        | DivExpr(a, b) -> DivExpr(applyMatches a matches, applyMatches b matches)
        | PowExpr(a, b) -> PowExpr(applyMatches a matches, applyMatches b matches)
        | Number(n) -> Number(n)
        | Variable(s) -> (List.find (fun (m: Match) -> m.Name = s) matches).Expression


// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.Experimental.Symbolics
open Fetisch
open System

type Token =
    | Illegal of string
    | Whitespace
    | Eof
    | Plus
    | Minus
    | Mul
    | Div
    | Pow
    | NumberLiteral of bigint
    | Identifier of string
    | RndOpen
    | RndClose

module ExprParser =
    let tokenizeString (s: string) : Token list =
        let rec (|IsWhiteSpace|_|) = function
            | x::t when x = ' ' ->
                match t with
                | IsWhiteSpace (n, nt) ->
                    Some (String.Concat(x, n), nt)
                | _ ->
                    Some ((sprintf "%c" x), t)
            | _ ->
                None
        let rec (|IsNumber|_|) = function
            | x::t when x >= '0' && x <= '9' ->
                match t with
                | IsNumber (n, nt) ->
                    Some (String.Concat(x, n), nt)
                | _ ->
                    Some ((sprintf "%c" x), t)
            | _ ->
                None
        let rec tokenize (s: char list) : Token list =
            match s with
            | [] -> [Eof]
            | IsWhiteSpace (_, tail) -> tokenize tail
            | IsNumber (num, tail) -> NumberLiteral(bigint.Parse(num)) :: (tokenize tail)
            | '*' :: tail -> Mul :: (tokenize tail)
            | '+' :: tail -> Plus :: (tokenize tail)
            | '(' :: tail -> RndOpen :: (tokenize tail)
            | ')' :: tail -> RndClose :: (tokenize tail)
            | _ -> [Illegal (String.Concat (Array.ofList s))]
        tokenize (Seq.toList s)

    let parseString (s: string) : Expression =
        /// Consumes token @p expected from token list @p tokens
        /// @returns new token list with first token consumed (removed).
        let consumeToken (tokens: Token list) (expected: Token): Token list =
            match tokens with
            | [] -> failwithf "Unexpected end of token stream. Expected %A instead." expected
            | [head] when head = expected -> []
            | head :: tail when head = expected -> tokens.Tail
            | unexpected :: tail -> failwithf "Unexpected token %A. Expected %A instead." unexpected expected
        /// Parses input into an expression.
        /// @returns a pair of a parsed expression and remaining input tokens.
        let rec parseExpr (tokens: Token list): Expression * Token list =
            let lhs, t = parseTerm tokens
            match t.Head with
            | Mul -> match parseTerm t.Tail with | (rhs, rem) -> (lhs * rhs, rem)
            | Div -> match parseTerm t.Tail with | (rhs, rem) -> (lhs / rhs, rem)
            | _ -> (lhs, t)
        /// Parses a term into an expression.
        /// @returns a pair of a parsed expression and remaining input tokens.
        and parseTerm (tokens: Token list): Expression * Token list =
            let lhs, t = parsePrimary tokens
            match t.Head with
            | Plus  -> match parseTerm t.Tail with | (rhs, rem) -> (lhs + rhs, rem)
            | Minus -> match parseTerm t.Tail with | (rhs, rem) -> (lhs - rhs, rem)
            | _ -> (lhs, t)
        /// Parses a primary expression
        /// @returns a pair of a parsed expression and remaining input tokens.
        and parsePrimary (tokens: Token list): Expression * Token list =
            match tokens.Head with
            | NumberLiteral n ->
                (Number (BigRational.FromBigInt n), tokens.Tail)
            | RndOpen ->
                let (e, tail) = parseExpr tokens.Tail
                (e, consumeToken tail RndClose)
            | _ -> failwithf "Invalid Token: %A" tokens.Head
        match tokenizeString s |> parseExpr with
        | (expr, []) -> expr
        | (_, tokens) -> failwithf "Unexpected trailing tokens: %A" tokens.Head

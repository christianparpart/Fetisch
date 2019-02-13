// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.SymbolicAlgebra

module ExprParser =
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

    type CharStream(_s: string) = class
        let input = _s
        let mutable pos: int = 0

        member self.IsEof() : bool = pos = input.Length
        member self.CurrentChar() : char = input.[pos]
        member self.Position() : int = pos
        member self.Advance() =
            assert(pos < input.Length)
            pos <- pos + 1
    end

    type Scanner(_stream: CharStream) as self =
        class
            let stream = _stream
            let mutable literal : string = ""
            let mutable token : Token = Token.Illegal

            do self.Advance() |> ignore

            member self.CurrentToken() = token
            member self.CurrentLiteral() = literal

            override self.ToString() : string =
                let pos = stream.Position()
                let stok = token.ToString()
                sprintf "%5i: %s \"%s\"" pos stok literal

            member self.Advance() : Token =
                self.AdvanceOne()
                while token = Token.Whitespace do
                    self.AdvanceOne()
                token

            member private self.AdvanceOne() : unit =
                literal <- ""
                token <-
                    if stream.IsEof() then
                        Token.Eof
                    else
                        match stream.CurrentChar() with
                        | '+' -> stream.Advance(); Token.Plus
                        | '-' -> stream.Advance(); Token.Minus
                        | '*' -> stream.Advance(); Token.Mul
                        | '/' -> stream.Advance(); Token.Div
                        | '^' -> stream.Advance(); Token.Pow
                        | '(' -> stream.Advance(); Token.RndOpen
                        | ')' -> stream.Advance(); Token.RndClose
                        | c when c = ' ' || c = '\t' ->
                            stream.Advance()
                            Token.Whitespace
                        | n when n >= '0' && n <= '9' ->
                            while (not(stream.IsEof()) && stream.CurrentChar() >= '0' && stream.CurrentChar() <= '9') do
                                literal <- literal + System.String.Concat([stream.CurrentChar()])
                                stream.Advance()
                            Token.NumberLiteral
                        | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')  ->
                            // TODO: multi char identifiers
                            literal <- literal + (string c)
                            stream.Advance()
                            Token.Identifier
                        | _ ->
                            stream.Advance()
                            Token.Illegal
        end

    exception ParserError of string

    type Parser(_scanner: Scanner) = class
        let scanner = _scanner

        member self.Parse() : Expr =
            let e = self.Expr()
            self.ConsumeToken(Token.Eof)
            e

        member private self.Expr() : Expr = self.AddExpr()

        member private self.AddExpr() : Expr =
            let mutable lhs = self.NegExpr()
            let mutable br = false
            while not br do
                match self.CurrentToken() with
                | Token.Plus -> self.AdvanceToken(); lhs <- AddExpr(lhs, self.NegExpr())
                | Token.Minus -> self.AdvanceToken(); lhs <- SubExpr(lhs, self.NegExpr())
                | _ -> br <- true
            lhs

        member private self.NegExpr() : Expr =
            match self.CurrentToken() with
            | Token.Minus ->
                self.AdvanceToken()
                NegExpr(self.NegExpr())
            | _ ->
                self.MulExpr()

        member private self.MulExpr() : Expr =
            let mutable lhs = self.PowExpr()
            let mutable br = false
            while not br do
                match self.CurrentToken() with
                | Token.Mul -> self.AdvanceToken(); lhs <- MulExpr(lhs, self.PowExpr())
                | Token.Div -> self.AdvanceToken(); lhs <- DivExpr(lhs, self.PowExpr())
                | _ -> br <- true
            lhs

        member private self.PowExpr() : Expr =
            let mutable lhs = self.PrimaryExpr()
            let mutable br = false
            while not br do
                match self.CurrentToken() with
                | Token.Pow -> self.AdvanceToken(); lhs <- PowExpr(lhs, self.PrimaryExpr())
                | _ -> br <- true
            lhs

        member private self.PrimaryExpr() : Expr =
            match self.CurrentToken() with
            | Token.NumberLiteral ->
                let expr = NumberExpr(bigint.Parse(scanner.CurrentLiteral()))
                self.AdvanceToken()
                expr
            | Token.Identifier ->
                let expr = SymbolExpr(scanner.CurrentLiteral())
                self.AdvanceToken()
                expr
            | Token.RndOpen ->
                self.AdvanceToken()
                let expr = self.Expr()
                self.ConsumeToken(Token.RndClose)
                expr
            | _ -> raise(ParserError("Invalid primary expression"))

        member private self.ConsumeToken(expected: Token) : unit =
            if self.CurrentToken() <> expected then
                raise(ParserError(sprintf "Expected token %A but got %A" expected (self.CurrentToken())))
            self.AdvanceToken()

        member private self.CurrentToken() = scanner.CurrentToken()
        member private self.AdvanceToken() = scanner.Advance() |> ignore
    end

    let parseString (s: string): Expr =
        let parser = new Parser(new Scanner(new CharStream(s)))
        parser.Parse()


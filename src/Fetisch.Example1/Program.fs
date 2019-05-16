// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

open System
open Fetisch
open Fetisch.Algebra
open Fetisch.LinearAlgebra
open Fetisch.Experimental.Symbolics
open FSharp.Charting

do Console.OutputEncoding <- Text.Encoding.UTF8

#if false
let trySimplify exprStr =
    let e1 = ExprParser.parseString exprStr
    let e2 = Simplifier.simplify e1
    let s2 = e2.ToString()
    printfn "%*s%A = %A" ((max 15 (s2.Length)) - s2.Length) " " e2 e1

let misc () =
    // Aufgabe 1, Matrix B
    let λ = SymbolExpr("λ")
    let a = Matrix.create [[2G*λ;  λ; -1G; 3G*λ];
                           [  0G; 0G; -2G;   1G];
                           [   λ; 1G; -1G;   0G];
                           [2G*λ; 0G;  2G;   1G]];

    //printfn "%s" (TextFormatter.formatMatrix a)

    //let b = Solver.rowCanonicalForm(a)
    //printfn "%s" (TextFormatter.formatMatrix b)

    // XXX for automatic simplification I could create a SimplifiedSymbolicAlgebra
    // or WrappedAlgebra< ^F> that would auto-simplify upon each operation.
    //let b' = Matrix.map b Simplifier.simplify
    //printfn "%s" (TextFormatter.formatMatrix b')

    let d = Matrix.determinant a
    printfn "det(A) = %A" d
    printfn "det(A) = %A" (Simplifier.simplify d)

    //let a = Matrix.createQ [[1; 0; 0; -1];
    //                        [3; 0; 0;  5];
    //                        [2; 2; 4; -3];
    //                        [1; 0; 5;  0]];
    //printfn "%s" (TextFormatter.formatMatrix a)
    //printfn "det(A): %s" ((Matrix.determinant a).ToString())
#endif

let elementary_matrix_decomposition () =
    //let A = Matrix.createQ [[1; 2; 3]; [0; 1; 4]; [5; 6; 0]];
    //let I = Matrix.init 3 3 (fun i j -> if i = j then 1N else 0N)
    let I = Matrix.init 3 3 (fun i j -> if i = j then 1G else 0G)
    let A = Matrix.create [[1G; 2G; 3G]; [0G; 1G; 4G]; [5G; 6G; 0G]]

    printfn "     I = %s" (I.AsString())
    printfn "     A = %s" (A.AsString())

    let C = Solver.decompose A
    List.iteri (fun (i: int) (c: Matrix<_>) -> printfn "   C_%d = %s" (1 + i) (c.AsString())) C

    let A' = List.fold (*) I C
    printfn "    A' = %s = C_0 * ... * C_%d" (A'.AsString()) (List.length C)

    // FIXME: why's that not compiling (SRTP FU)
    //let A2' = Matrix.inverse A
    //printfn "    A''= %s" (A2'.AsString())

    let A_times_A' = A * A'
    printfn "A * A' = %s" (A_times_A'.AsString())

[<EntryPoint>]
let main argv =
    elementary_matrix_decomposition()
    //let t = Fetisch.Experimental.Symbolics.ExprParser.tokenizeString "12 * (3 + 4)"
    //printfn "t: %A" t
    //Experimental.Symbolics.Test.main()
    //Console.ReadKey() |> ignore
    0


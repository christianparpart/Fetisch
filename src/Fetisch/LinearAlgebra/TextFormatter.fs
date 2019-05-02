// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.LinearAlgebra

module TextFormatter =
    open Matrix
    open Solver
    open Fetisch.Util

    let inline formatMatrix (mat: Matrix<_>) : string =
        let mkRow acc i =
            let mkCol acc j =
                let minWidth = 3
                let columnWidthAtColumn =
                    let rec width = function
                        | i when i > (rowCount mat) -> minWidth
                        | i -> max (mat.[i, j].ToString().Length) (width (i + 1))
                    width 1
                acc + (sprintf " %*s" columnWidthAtColumn (mat.[i, j].ToString()))
            acc + " " + (Array.fold mkCol "" [| 1 .. columnCount mat |]) + "\n"
        Array.fold mkRow "" [| 1 .. rowCount mat |]

    let inline formatOperation (operation: Operation< ^F>) : string =
        let abs num =
            match num with
            | num when num < (num - num) -> -num
            | num -> num
        let addOpFor num =
            match num with
            | n when n < (num - num) -> "-"
            | _ -> "+"
        match operation with
        | SwapRow(a, b) -> (sprintf "Swap row %s with %s" (roman a) (roman b))
        | ScaleRow(row, scalar) -> (sprintf "Scale row %s by %s" (roman row) (scalar.ToString()))
        | AddScaledRow(targetRow, scalar, row) ->
            match scalar with
            | s when isOne s -> sprintf "%s + %s" (roman targetRow) (roman row)
            | s when isMinusOne s -> sprintf "%s - %s" (roman targetRow) (roman row)
            | _ -> sprintf "%s %s %s * %s" (roman targetRow) (addOpFor scalar) (abs(scalar).ToString()) (roman row)
        | SwapColumn(a, b) -> (sprintf "Swap column %d with %d" a b)
        | ScaleColumn(a, scalar) -> (sprintf "Scale column %d by %s" a (scalar.ToString()))
        | AddScaledColumn(targetColumn, scalar, column) -> (sprintf "Column %d = %s * %d" targetColumn (scalar.ToString()) column)

    let inline formatSteps(steps: Step< ^F> list) : string =
        let rec formatStep list n =
            match list with
            | [] ->
                ""
            | head :: tail ->
                let act = sprintf "%d.) %s\n\n" n (formatOperation (head.Operation))
                let mat = formatMatrix (head.Matrix)
                let next = formatStep tail (n + 1)
                act + mat + "\n" + next
        formatStep steps 1

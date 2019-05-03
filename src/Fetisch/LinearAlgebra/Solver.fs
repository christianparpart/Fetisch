// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.LinearAlgebra

open Fetisch.Util

open FSharp.Core.LanguagePrimitives

// TODO: add solveIterative(A, b)

module Solver =
    open Matrix

    // -----------------------------------------------------------------------------------
    // Elementary Matrix Operations

    let inline swapRow(mat: Matrix<_>) (a: int) (b: int) =
        init (rowCount mat) (columnCount mat)
             (fun i j -> match i with
                         | row when row = a -> mat.[b, j]
                         | row when row = b -> mat.[a, j]
                         | _ -> mat.[i, j])

    let inline scaleRow (mat: Matrix< ^F>) (row: int) (scalar: ^F) =
        init (rowCount mat) (columnCount mat)
             (fun i j -> if i = row then mat.[i, j] * scalar
                         else mat.[i, j])

    let inline addScaledRow (mat: Matrix< ^F>) (targetRow: int) (scalar: ^F) (row: int) =
        init (rowCount mat) (columnCount mat)
             (fun i j -> if i = targetRow then scalar * mat.[row, j] + mat.[i, j]
                         else mat.[i, j])

    let inline swapColumn(mat: Matrix<_>) (a: int) (b: int) =
        init (rowCount mat) (columnCount mat)
             (fun i j -> match j with
                         | col when col = a -> mat.[i, b]
                         | col when col = b -> mat.[i, a]
                         | _ -> mat.[i, j])

    let inline scaleColumn(mat: Matrix< ^F>) (column: int) (scalar: ^F) =
        init (rowCount mat) (columnCount mat)
             (fun i j -> if j = column then scalar * mat.[i, j]
                         else mat.[i, j])

    let inline addScaledColumn(mat: Matrix< ^F>) (targetColumn: int) (scalar: ^F) (column: int) =
        init (rowCount mat) (columnCount mat)
             (fun i j -> if j = targetColumn then mat.[i, j] + scalar * mat.[i, column]
                         else mat.[i, j])

    type Step< ^F when ^F : equality
                   and ^F : (static member ( * ): ^F * ^F -> ^F)
                   and ^F : (static member ( + ): ^F * ^F -> ^F)
             > = {
        Operation : ElementaryOperation< ^F>
        Matrix : Matrix< ^F>
    }

    type State< ^F when ^F : equality
                    and ^F : (static member ( * ): ^F * ^F -> ^F)
                    and ^F : (static member ( + ): ^F * ^F -> ^F)
              > =
        | InlineState of matrix: Matrix< ^F>
        | IterativeState of Tip: Matrix< ^F> * Steps: Step< ^F> list

    let inline matrixState (handler: State< ^F>) : Matrix< ^F> =
        match handler with
        | InlineState(m) -> m
        | IterativeState(tip, _) -> tip

    let inline updateState (operation: ElementaryOperation< ^F>) (work: State< ^F>) : State< ^F> =
        match work with
        | InlineState (mat) ->
            InlineState(operation * mat)
        | IterativeState (mat, steps) ->
            let mat' = operation * mat
            let step = { Operation = operation; Matrix = mat' }
            IterativeState(mat', steps @ [step])

    // find smallest index k with m_{k}{l} being the smallest value in column l that is not 0.
    let inline findSmallestAtColumnFor (mat: Matrix< ^F>) (i0: int) (j: int) : Option<int> =
        let finder (result: Option<int>) (i: int) =
            match result with
            | Some k ->
                let k' = mat.[k, j]
                let i' = mat.[i, j]
                if isNotOne k' && (isOne (abs i') || (isNotZero i' && abs i' < abs k')) then
                    Some i
                else
                    result
            | None ->
                if isNotZero mat.[i, j] then
                    Some i
                else
                    None
        Array.fold finder None [| i0 .. rowCount mat |]

    let inline private ensureValueOneAt (state: State< ^F>) (i: int) (j: int) : State< ^F> =
        let mat = matrixState state
        if mat.[i, j] = GenericOne then
            state
        else
            match findSmallestAtColumnFor mat i j with
            | Some k when k <> i ->
                if mat.[k, j] <> GenericOne then
                    state
                    |> updateState (SwapRow(i, k))
                    |> updateState (ScaleRow(i, multiplicativeInverse mat.[k, j]))
                else
                    state
                    |> updateState (SwapRow(i, k))
            | Some k ->
                if mat.[k, j] <> GenericOne then
                    state
                    |> updateState (ScaleRow(i, multiplicativeInverse mat.[k, j]))
                else
                    state
            | None ->
                state

    // Tests whether or not given row in given matrix only contains zeros.
    let inline private isNonNullRow (mat: Matrix< ^F>) (i: int) : bool =
        Array.exists (fun v -> isNotZero v) (mat.Row(i).Values)

    let inline private makeZerosAbove (i: int) (j: int) (work: State< ^F>) : State< ^F> =
        let rec makeZero (k: int) (work: State< ^F>) : State< ^F> =
            match k >= 1 with
            | false ->
                work
            | true ->
                let mat = matrixState work
                match mat.[k, j] <> GenericZero && (isNonNullRow mat k) with
                | true ->
                    work
                    |> updateState (AddScaledRow(k, -mat.[k, j], i))
                    |> makeZero (k - 1)
                | false ->
                    makeZero (k - 1) work
        makeZero (i - 1) work

    // Set all numbers to zero that are in mat[i..m, j].
    let inline private makeZerosBelow (i: int) (j: int) (work: State< ^F>) : State< ^F> =
        let rec makeZero (k: int) (work: State< ^F>) : State< ^F> =
            let mat = matrixState work
            match k <= rowCount mat with
            | false ->
                work
            | true ->
                match mat.[k, j] <> GenericZero && (isNonNullRow mat k) with
                | true ->
                    work
                    |> updateState (AddScaledRow(k, -(mat.[k, j] / mat.[i, j]), i))
                    |> makeZero (k + 1)
                | false ->
                    makeZero (k + 1) work
        makeZero (i + 1) work

    let inline private rowEchelonStep (d: int) (work: State< ^F>) : State< ^F> =
        let rec step (d: int) (work: State< ^F>) : State< ^F> =
            let inline ensureNonZeroValueAt (state: State< ^F>) (i: int) (j: int) : State< ^F> =
                let inline findNonZeroAtColumnFor (mat: Matrix< ^F>) (i0: int) (j: int) : Option<int> =
                    match Array.tryFindIndex (fun i -> isNotZero mat.[i, j]) [| i0 .. rowCount mat |] with
                    | Some i -> Some (i0 + i)
                    | None -> None
                let mat = matrixState state
                if mat.[i, j] = GenericOne then
                    state
                else
                    match findNonZeroAtColumnFor mat i j with
                    | Some k when k <> i ->
                        state |> updateState (SwapRow(i, k))
                    | _ ->
                        state
            let mat = matrixState work
            if d > min (rowCount mat) (columnCount mat) then
                work
            else
                ensureNonZeroValueAt work d d
                |> makeZerosBelow d d
                |> step (d + 1)
        step d work

    // Transforms given matrix into row echelon form, preserving each step.
    let inline rowEchelonFormIterative (mat: Matrix< ^F>) : Step< ^F> list =
        match rowEchelonStep 1 (IterativeState(mat, [])) with
        | IterativeState(tip, steps) -> steps
        | _ -> invalidOp "The impossible is happening."

    let inline private rowCanonicalStep (d: int) (work: State< ^F>) : State< ^F> =
        let rec step (d: int) (work: State< ^F>) : State< ^F> =
            let mat = matrixState work
            if d > min (rowCount mat) (columnCount mat) then
                work
            else
                ensureValueOneAt work d d
                |> makeZerosBelow d d
                |> makeZerosAbove d d
                |> step (d + 1)
        step d work

    // Transforms given matrix to row canonical form, preserving each step.
    let inline rowCanonicalFormIterative (mat: Matrix< ^F>) : Step< ^F> list =
        match rowCanonicalStep 1 (IterativeState(mat, [])) with
        | IterativeState(tip, steps) -> steps
        | _ -> invalidOp "The impossible is happening."

    // Transforms given matrix to row echelon form.
    let inline rowEchelonForm (mat: Matrix< ^F>) : Matrix< ^F> =
        (List.last (rowEchelonFormIterative mat)).Matrix

    // Transforms given matrix to row canonical form.
    let inline rowCanonicalForm (mat: Matrix< ^F>) : Matrix< ^F> =
        match rowCanonicalStep 1 (InlineState(mat)) with
        | InlineState(mat) -> mat
        | _ -> invalidOp "The impossible is happening."

    // Solves linear system of equations: Ax=b. Reveives A and b, returns x.
    let inline solve (mat: Matrix< ^F>) (b: Vector< ^F>) : Vector< ^F> =
        let m_ext = mat +|+ b
        let m' = rowCanonicalForm m_ext
        m'.Column (columnCount m')


﻿// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
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
                   and ^F : (static member One: ^F)
             > = {
        Operation : ElementaryOperation< ^F>
        Matrix : Matrix< ^F>
    }

    // Extracts the list of elementary operations.
    let inline elementaryOperations (steps: Step< ^F> list): ElementaryOperation< ^F> list =
        let rec getOps (steps: Step< ^F> list) (ops: ElementaryOperation< ^F> list): ElementaryOperation< ^F> list =
            match steps with
            | [] -> ops
            | s :: xs -> s.Operation :: (getOps xs ops)
        getOps steps []

    type State< ^F when ^F : equality
                    and ^F : (static member ( * ): ^F * ^F -> ^F)
                    and ^F : (static member ( + ): ^F * ^F -> ^F)
                    and ^F : (static member One: ^F)
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

    /// Transforms given matrix into row echelon form, preserving each step.
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

    /// Transforms given matrix to row canonical form, preserving each step.
    let inline rowCanonicalFormIterative (mat: Matrix< ^F>) : Step< ^F> list =
        match rowCanonicalStep 1 (IterativeState(mat, [])) with
        | IterativeState(tip, steps) -> steps
        | _ -> invalidOp "The impossible is happening."

    /// Transforms given matrix to row echelon form.
    let inline rowEchelonForm (mat: Matrix< ^F>) : Matrix< ^F> =
        (List.last (rowEchelonFormIterative mat)).Matrix

    // Transforms given matrix to row canonical form.
    let inline rowCanonicalForm (mat: Matrix< ^F>) : Matrix< ^F> =
        match rowCanonicalStep 1 (InlineState(mat)) with
        | InlineState(mat) -> mat
        | _ -> invalidOp "The impossible is happening."

    /// Solves linear system of equations: Ax=b. Reveives A and b, returns x.
    let inline solve (mat: Matrix< ^F>) (b: Vector< ^F>) : Vector< ^F> =
        let m_ext = mat +|+ b
        let m' = rowCanonicalForm m_ext
        m'.Column (columnCount m')

    /// Constructs a left-multiplyable m*m elementary matrix equivalent to the given elementary operation.
    let inline elementaryMatrix (m: int) (op: ElementaryOperation< ^F>): Matrix< ^F> =
        let kroneckerDelta (i: int) (j: int): ^F =
            if i = j then GenericOne
            else GenericZero
        match op with
        | SwapRow(a, b) ->
            let init i j =
                if i = a then kroneckerDelta b j
                elif i = b then kroneckerDelta a j
                else kroneckerDelta i j
            Matrix.init m m init
        | AddScaledRow(targetRow, scalar, row) ->
            let init i j =
                if i = targetRow && j = row then scalar * kroneckerDelta j j
                else kroneckerDelta i j
            Matrix.init m m init
        | ScaleRow(row, scalar) ->
            let init i j =
                if i = row then scalar * kroneckerDelta i j
                else kroneckerDelta i j
            Matrix.init m m init
        | SwapColumn(a, b) ->
            failwith "TODO: Elementary operation SwapColumn not yet implemented."
        | AddScaledColumn(targetColumn, scalar, column) ->
            failwith "TODO: Elementary operation AddScaledColumn not yet implemented."
        | ScaleColumn(column, scalar) ->
            failwith "TODO: Elementary operation ScaleColumn not yet implemented."

    /// Constructs a list of elementary m*m matrices for given elementary operations.
    let inline elementaryMatrices (m: int) (ops: ElementaryOperation< ^F> list): Matrix< ^F> list =
        let folder (mats: Matrix< ^F> list) (op: ElementaryOperation< ^F>): Matrix< ^F> list =
            let e = elementaryMatrix m op
            e :: mats
        List.fold folder [] ops

    /// Constructs a list of elementary m*m matrices whos product constructs the given input matrix.        
    ///
    /// The matrix must be regular (invertible) in order to be elementary matrix constructible.
    let inline decompose (mat: Matrix< ^F>): Matrix< ^F> list =
        let m = Matrix.rowCount mat
        if not (Matrix.isQuadratic mat) then
            failwithf "Matrix must be square (and regular) but is a %dx%d matrix." m m
        let I =
            let init (i: int) (j: int): ^F =
                if i = j then GenericOne
                else GenericZero
            Matrix.init m m init
        let steps = rowCanonicalFormIterative mat
        let ops = elementaryOperations steps
        let C = elementaryMatrices (Matrix.rowCount mat) ops
        C

    // FIXME: I cannot multiply two matrices in this function? (op_Multiply not found)
    //let inline inverse (mat: Matrix< ^F>): Matrix< ^F> =
    //    let m = Matrix.rowCount mat
    //    let I =
    //        let init (i: int) (j: int): ^F =
    //            if i = j then GenericOne
    //            else GenericZero
    //        Matrix.init m m init
    //    let steps = rowCanonicalFormIterative mat
    //    let ops = elementaryOperations steps
    //    let C : Matrix< ^F> list = elementaryMatrices (Matrix.rowCount mat) ops
    //    List.fold (*) I C

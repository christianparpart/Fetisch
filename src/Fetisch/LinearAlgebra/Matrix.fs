// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.LinearAlgebra

open Fetisch.Util
open FSharp.Core.LanguagePrimitives

// TODO: type Matrix< ^F when ^F : equality> (values: ^F [,]) =
type Matrix<'F when 'F : equality> (values: 'F [,]) =
    member val Values = values

    member inline private this.RowCount = Array2D.length1 this.Values
    member inline private this.ColumnCount = Array2D.length2 this.Values

    member inline this.Item with get (i: int, j: int) = this.Values.[i - 1, j - 1]
    member inline this.Row with get (i: int) = Vector.init (this.ColumnCount) (fun j -> this.[i, j])
    member inline this.Column with get (j: int) = Vector.init (this.RowCount) (fun i -> this.[i, j])

    static member inline Init (m: int) (n: int) (init: int -> int -> ^F) =
        Matrix(Array2D.init m n (fun i j -> init (i + 1) (j + 1)))

    override mat.ToString() =
        let mkRow acc i =
            let width = 1
            let mkCol acc j = acc + (sprintf " %*s" width (mat.[i, j].ToString()))
            let sep = if i > 1 then ", " else ""
            acc + sep + "{" + (Array.fold mkCol "" [| 1 .. mat.ColumnCount |]) + "}"
        Array.fold mkRow "" [| 1 .. mat.RowCount |]

    member inline this.Slice(row, column, m, n) =
        Matrix< ^F>.Init m n (fun i j -> this.Values.[row + i - 2, column + j - 2])

    // example-usage: mat.[rowStart .. rowEnd, columnStart .. columnEnd]
    member inline this.GetSlice(rowStart, rowEnd, columnStart, columnEnd) =
        let rstart = defaultArg rowStart 1
        let cstart = defaultArg columnStart 1
        let rcount = (defaultArg rowEnd (this.RowCount)) - rstart + 1
        let ccount = (defaultArg columnEnd (this.ColumnCount)) - cstart + 1
        this.Slice(rstart, cstart, rcount, ccount)

    /// Concats two matrixes together with a being on the left and b being on the right hand side.
    static member inline ( +|+ ) (a: Matrix< ^F>, b: Matrix< ^F>) : Matrix< ^F> =
        if not (a.RowCount = b.RowCount) then
            invalidArg "a" "Both matrices must match in row count"
        Matrix< ^F>.Init (a.RowCount) (a.ColumnCount + b.ColumnCount)
                         (fun i j -> if j <= a.ColumnCount then a.[i, j]
                                     else b.[i, j - a.ColumnCount])

    // Concats a matrix with a vector on the right side.
    static member inline ( +|+ ) (mat: Matrix< ^F>, b: Vector< ^F>): Matrix< ^F> =
        Matrix< ^F>.Init (mat.RowCount) (mat.ColumnCount + 1)
                         (fun i j -> if j <= mat.ColumnCount then mat.[i, j]
                                     else b.[i])

    // Adds two matrices.
    static member inline ( + ) (a: Matrix< ^G>, b: Matrix< ^G>) : Matrix< ^G> =
        if not (a.RowCount = b.RowCount && a.ColumnCount = b.ColumnCount) then
            invalidArg "a" "Both matrices must match in row and column count"
        Matrix< ^G>.Init (a.RowCount) (a.ColumnCount) (fun i j -> a.[i, j] + b.[i, j])

    // Subtracts right matrix from left matrix.
    static member inline ( - ) (a: Matrix< ^G>, b: Matrix< ^G>) : Matrix< ^G> =
        if not (a.RowCount = b.RowCount && a.ColumnCount = b.ColumnCount) then
            invalidArg "a" "Both matrices must match in row and column count"
        Matrix< ^G>.Init (a.RowCount) (a.ColumnCount) (fun i j -> a.[i, j] - b.[i, j])

    // Matrix * Matrix multiplication
    static member inline ( * ) (a: Matrix< ^G>, b: Matrix< ^G>) : Matrix< ^G> =
        if not (a.ColumnCount = b.RowCount) then
            invalidArg "a" "Cannot perform matrix multiplication."
        Matrix< ^G>.Init (a.RowCount) (b.ColumnCount)
                         (fun i j -> Array.fold (fun acc l -> acc + a.[i, l] * b.[l, j])
                                                (additiveIdentity a.[1, 1])
                                                [| 1 .. a.ColumnCount |])

    // Matrix * Vector mulitplication
    static member inline ( * ) (a: Matrix< ^G>, b: Vector< ^G>) : Vector< ^G> =
        if not (a.ColumnCount = b.Dimension) then
            invalidArg "a" "Cannot perform matrix multiplication."
        Vector.init (b.Dimension)
                    (fun i -> Array.fold (fun acc j -> acc + b.[j] * a.[i, j])
                                         (additiveIdentity a.[1, 1])
                                         [| 1 .. a.ColumnCount |])

    // Scalar * Matrix multiplication
    static member inline ( * ) (mat: Matrix< ^G>, scalar: ^G) : Matrix< ^G> =
        Matrix< ^G>.Init (mat.RowCount) (mat.ColumnCount) (fun i j -> scalar * mat.[i, j])

//type SparseMatrix(items) =
//    do printfn "TODO"

module Matrix =
    open Fetisch // BigRational

    let inline init (m: int) (n: int) (init: (int -> int -> ^F)) =
        Matrix< ^F>(Array2D.init m n (fun i j -> init (i + 1) (j + 1)))

    let inline create (values: ^F list list) : Matrix< ^F> =
        Matrix< ^F>(array2D values)

    /// Creates a Matrix of scalar type BigRational with given integer 2D array.
    let inline createQ (values: int list list) : Matrix<BigRational> =
        let values' = array2D values
        let m = Array2D.length1 values'
        let n = Array2D.length2 values'
        Matrix<_>(Array2D.init m n (fun i j -> BigRational (values'.[i, j])))

    /// Creates a Matrix of scalar type float with given integer 2D array.
    let inline createF (values: int list list) =
        let values' = array2D values
        let m = Array2D.length1 values'
        let n = Array2D.length2 values'
        Matrix<_>(Array2D.init m n (fun i j -> float (values'.[i, j])))

    /// Retrieves the number of rows of the given matrix.
    let inline rowCount (mat: Matrix<_>) =
        Array2D.length1 (mat.Values)

    let inline row (mat: Matrix< ^F>) (i: int): Vector< ^F> =
        mat.Row i

    let inline column (mat: Matrix< ^F>) (j: int): Vector< ^F> =
        mat.Column j

    /// Retrieves the number of columns of the given matrix.
    let inline columnCount (mat: Matrix<_>) =
        Array2D.length2 (mat.Values)

    /// Tests whether or not given matrix is quadratic (row count equals column count)
    let inline isQuadratic (mat: Matrix<_>) =
        rowCount mat = columnCount mat

    /// Retrieves the element of the given matrix at row i and column i.
    let inline at (mat: Matrix<_>) i j =
        mat.[i, j]

    // Constructs a new matrix with each element "mapped".
    let inline map (mat: Matrix< ^F>) (mapper: (^F -> ^G)) : Matrix< ^G> =
        init (rowCount mat) (columnCount mat) (fun i j -> mapper (mat.[i, j]))

    /// Computes the trace of the given matrix.
    let inline trace (mat: Matrix< ^F>) =
        if not (isQuadratic mat) then
            invalidArg "mat" "Matrix must be quadratic to compute its trace."
        Array.fold (fun acc i -> acc + mat.[i, i]) GenericZero [| 1 .. min (rowCount mat) (columnCount mat) |]

    /// Computes the transposed matrix of the given input matrix.
    let inline transpose (mat: Matrix< ^F>) : Matrix< ^F> =
        init (columnCount mat) (rowCount mat) (fun i j -> mat.[j, i])

    /// Constructs the algebraic complement matrix of the matrix element at given line i and column j.
    let inline complement (mat: Matrix< ^G>) (i: int) (j: int) : Matrix< ^G> =
        let m = -1 + rowCount mat
        let n = -1 + columnCount mat
        let initf k l =
            match k < i with
            | true ->
                match l < j with
                | true -> mat.[k, l]
                | false -> mat.[k, l + 1]
            | false ->
                match l < j with
                | true -> mat.[k + 1, l]
                | false -> mat.[k + 1, l + 1]
        init m n initf

    /// Computes the determinant of given matrix.
    let inline determinant (mat: Matrix< ^F>) : ^F =
        let rec det (mat: Matrix< ^F>) : ^F =
            match rowCount mat with
            | 1 ->
                mat.[1, 1]
            | 2 ->
                let a = mat.[1, 1] * mat.[2, 2]
                let b = mat.[2, 1] * mat.[1, 2]
                a - b
            | 3 -> 
                let a = mat.[1, 1] * mat.[2, 2] * mat.[3, 3]
                      + mat.[1, 2] * mat.[2, 3] * mat.[3, 1]
                      + mat.[1, 3] * mat.[2, 1] * mat.[3, 2]
                let b = mat.[1, 3] * mat.[2, 2] * mat.[3, 1]
                      + mat.[1, 1] * mat.[2, 3] * mat.[3, 2]
                      + mat.[1, 2] * mat.[2, 1] * mat.[3, 3]
                a - b 
            | _ ->
                // Using Laplace expansion.
                let dets = seq {
                    // TODO expand on the row/column with the most zero's (one's or two's -> smallest abs(nums))
                    // each row/col gets a score, which is the sum of its elements. elect row/col with best score
                    let i = 1
                    for j = 1 to columnCount mat do
                        let minor =
                            if i + j % 2 = 0
                            then +det (complement mat i j)
                            else -det (complement mat i j)
                        yield mat.[i, j] * minor
                }
                Seq.fold (+) GenericZero dets

        if not (isQuadratic mat) then
            invalidArg "mat" "Matrix must be quadratic to compute its trace."

        det mat

    // Computes the minor of a matrix element.
    let inline minor (mat: Matrix< ^G>) (i: int) (j: int) : ^G =
        // (-1)^(i + j) * det(complement mat i j)
        let det = determinant (complement mat i j)
        if i + j % 2 = 0
        then +det
        else -det

    // Computes the cofactor of a given element this matrix.
    let inline cofactorAt (mat: Matrix< ^G>) (i: int) (j: int) : ^G =
        mat.[i, j] * (minor mat i j)

    // Constructs a cofactor matrix, with each element to be the cofactor of the corresponding input matrix's element.
    let inline cofactor (mat: Matrix< ^G>) : Matrix< ^G> =
        init (rowCount mat) (columnCount mat) (cofactorAt mat)

    // The adjugate matrix is the transpose of a cofactor matrix.
    let inline adjugate (mat: Matrix< ^G>) : Matrix< ^G> =
        transpose (cofactor mat)

    // Constructs the inverse matrix using minor
    let inline inverse (mat: Matrix< ^G>) : Matrix< ^G> =
        let det = determinant mat
        if det = GenericZero then
            invalidArg "mat" "Matrix is not invertable"
        (GenericOne / det) * (adjugate mat)

    //let inline rank (mat: Matrix< ^F>): ^F = () // TODO

    let inline foldElements (folder: (^T -> ^F -> ^T)) (init: ^T) (mat: Matrix< ^F>): ^T =
        // TODO: rewrite to immutable (Array.fold?)
        let mutable y = init
        for i = 1 to rowCount mat do
            for j = 1 to columnCount mat do
                y <- folder y mat.[i, j]
        y

    let inline density (mat: Matrix< ^F>): float =
        let total = rowCount mat * columnCount mat
        let zeros = foldElements (fun a v -> if v <> GenericZero then a + 1 else a) 0 mat
        float zeros / float total

    let inline sparsity (mat: Matrix< ^F>): float =
        1.0 - (density mat)

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

    type RowIndex = int
    type ColumnIndex = int

    type Action< ^F> =
        | SwapRow of a: RowIndex * b: RowIndex
        | ScaleRow of row: RowIndex * scalar: ^F
        | AddScaledRow of targetRow: RowIndex * scalar: ^F * row: RowIndex
        | SwapColumn of a: RowIndex * b: RowIndex
        | ScaleColumn of a: ColumnIndex * scalar: ^F
        | AddScaledColumn of targetColumn: ColumnIndex * scalar: ^F * column: ColumnIndex

    // Applies a single elementary matrix transformation.
    let inline applyAction (mat: Matrix< ^F>) (action: Action< ^F>) : Matrix< ^F> =
        match action with
        | SwapRow(a, b) -> swapRow mat a b
        | ScaleRow(row, scalar) -> scaleRow mat row scalar
        | AddScaledRow(targetRow, scalar, row) -> addScaledRow mat targetRow scalar row
        | SwapColumn(a, b) -> swapColumn mat a b
        | ScaleColumn(column, scalar) -> scaleColumn mat column scalar
        | AddScaledColumn(targetColumn, scalar, column) -> addScaledColumn mat targetColumn scalar column

    type Step< ^F when ^F : equality> = {
        Action : Action< ^F>
        Matrix : Matrix< ^F>
    }

    type Worker< ^F when ^F : equality> =
        | InlineWorker of matrix: Matrix< ^F>
        | IterativeWorker of Tip: Matrix< ^F> * Steps: Step< ^F> list

    let inline workMatrix (handler: Worker< ^F>) : Matrix< ^F> =
        match handler with
        | InlineWorker(m) -> m
        | IterativeWorker(tip, _) -> tip

    let inline updateWork (action: Action< ^F>) (work: Worker< ^F>) : Worker< ^F> =
        match work with
        | InlineWorker (mat) ->
            InlineWorker(applyAction mat action)
        | IterativeWorker (mat, steps) ->
            let mat' = applyAction mat action
            let step = { Action = action; Matrix = mat' }
            IterativeWorker(mat', steps @ [step])

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

    let inline findNonZeroAtColumnFor (mat: Matrix< ^F>) (i0: int) (j: int) : Option<int> =
        match Array.tryFindIndex (fun i -> isNotZero mat.[i, j]) [| i0 .. rowCount mat |] with
        | Some i -> Some (i0 + i)
        | None -> None

    let inline ensureValueOneAt (worker: Worker< ^F>) (i: int) (j: int) : Worker< ^F> =
        let mat = workMatrix worker
        if mat.[i, j] = GenericOne then
            worker
        else
            match findSmallestAtColumnFor mat i j with
            | Some k when k <> i ->
                if mat.[k, j] <> GenericOne then
                    worker
                    |> updateWork (SwapRow(i, k))
                    |> updateWork (ScaleRow(i, multiplicativeInverse mat.[k, j]))
                else
                    worker
                    |> updateWork (SwapRow(i, k))
            | Some k ->
                if mat.[k, j] <> GenericOne then
                    worker
                    |> updateWork (ScaleRow(i, multiplicativeInverse mat.[k, j]))
                else
                    worker
            | None ->
                worker

    let inline ensureNonZeroValueAt (worker: Worker< ^F>) (i: int) (j: int) : Worker< ^F> =
        let mat = workMatrix worker
        if mat.[i, j] = GenericOne then
            worker
        else
            match findNonZeroAtColumnFor mat i j with
            | Some k when k <> i ->
                worker |> updateWork (SwapRow(i, k))
            | _ ->
                worker

    // Tests whether or not given row in given matrix only contains zeros.
    let inline isNonNullRow (mat: Matrix< ^F>) (i: int) : bool =
        Array.exists (fun v -> isNotZero v) (mat.Row(i).Values)

    let inline makeZerosAbove (i: int) (j: int) (work: Worker< ^F>) : Worker< ^F> =
        let rec makeZero (k: int) (work: Worker< ^F>) : Worker< ^F> =
            match k >= 1 with
            | false ->
                work
            | true ->
                let mat = workMatrix work
                match mat.[k, j] <> GenericZero && (isNonNullRow mat k) with
                | true ->
                    work
                    |> updateWork (AddScaledRow(k, -mat.[k, j], i))
                    |> makeZero (k - 1)
                | false ->
                    makeZero (k - 1) work
        makeZero (i - 1) work

    // Set all numbers to zero that are in mat[i..m, j].
    let inline makeZerosBelow (i: int) (j: int) (work: Worker< ^F>) : Worker< ^F> =
        let rec makeZero (k: int) (work: Worker< ^F>) : Worker< ^F> =
            let mat = workMatrix work
            match k <= rowCount mat with
            | false ->
                work
            | true ->
                match mat.[k, j] <> GenericZero && (isNonNullRow mat k) with
                | true ->
                    work
                    |> updateWork (AddScaledRow(k, -(mat.[k, j] / mat.[i, j]), i))
                    |> makeZero (k + 1)
                | false ->
                    makeZero (k + 1) work
        makeZero (i + 1) work

    let inline private rowEchelonStep (d: int) (work: Worker< ^F>) : Worker< ^F> =
        let rec step (d: int) (work: Worker< ^F>) : Worker< ^F> =
            let mat = workMatrix work
            if d > min (rowCount mat) (columnCount mat) then
                work
            else
                ensureNonZeroValueAt work d d
                |> makeZerosBelow d d
                |> step (d + 1)
        step d work

    // Transforms given matrix into row echelon form, preserving each step.
    let inline rowEchelonFormIterative (mat: Matrix< ^F>) : Step< ^F> list =
        match rowEchelonStep 1 (IterativeWorker(mat, [])) with
        | IterativeWorker(tip, steps) -> steps
        | _ -> invalidOp "The impossible is happening."

    let inline private rowCanonicalStep (d: int) (work: Worker< ^F>) : Worker< ^F> =
        let rec step (d: int) (work: Worker< ^F>) : Worker< ^F> =
            let mat = workMatrix work
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
        match rowCanonicalStep 1 (IterativeWorker(mat, [])) with
        | IterativeWorker(tip, steps) -> steps
        | _ -> invalidOp "The impossible is happening."

    // Transforms given matrix to row echelon form.
    let inline rowEchelonForm (mat: Matrix< ^F>) : Matrix< ^F> =
        (List.last (rowEchelonFormIterative mat)).Matrix

    // Transforms given matrix to row canonical form.
    let inline rowCanonicalForm (mat: Matrix< ^F>) : Matrix< ^F> =
        match rowCanonicalStep 1 (InlineWorker(mat)) with
        | InlineWorker(mat) -> mat
        | _ -> invalidOp "The impossible is happening."

    let inline solve (mat: Matrix< ^F>) (b: Vector< ^F>) : Vector< ^F> =
        let m_ext = mat +|+ b
        let m' = rowCanonicalForm m_ext
        m'.Column (columnCount m')

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

    let inline formatAction (action: Action< ^F>) : string =
        let abs num =
            match num with
            | num when num < (num - num) -> -num
            | num -> num
        let addOpFor num =
            match num with
            | n when n < (num - num) -> "-"
            | _ -> "+"
        match action with
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
                let act = sprintf "%d.) %s\n\n" n (formatAction (head.Action))
                let mat = formatMatrix (head.Matrix)
                let next = formatStep tail (n + 1)
                act + mat + "\n" + next
        formatStep steps 1

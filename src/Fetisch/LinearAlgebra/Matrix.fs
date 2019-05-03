// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

// TODO:
// - implement rank()
// - add API to retrieve all elementary matrices that can be multiplied in order to construct input matrix A.
// - extract API into its own Solver.fs
// - Matrix<'F> to Matrix< ^F>
// - SparseVector< ^F>

namespace Fetisch.LinearAlgebra

open Fetisch.Algebra
open Fetisch.Util

open FSharp.Core.LanguagePrimitives

type RowIndex = int
type ColumnIndex = int

type ElementaryOperation< ^G when ^G : equality
                              and ^G : (static member ( * ): ^G * ^G -> ^G)
                              and ^G : (static member ( + ): ^G * ^G -> ^G)
                        > =
    | SwapRow of a: RowIndex * b: RowIndex
    | ScaleRow of row: RowIndex * scalar: ^G
    | AddScaledRow of targetRow: RowIndex * scalar: ^G * row: RowIndex
    | SwapColumn of a: RowIndex * b: RowIndex
    | ScaleColumn of a: ColumnIndex * scalar: ^G
    | AddScaledColumn of targetColumn: ColumnIndex * scalar: ^G * column: ColumnIndex

type Matrix<'F when 'F : equality> (values: 'F [,]) =
    member val Values = values

    member inline private this.RowCount = Array2D.length1 this.Values
    member inline private this.ColumnCount = Array2D.length2 this.Values

    member inline this.Item with get (i: int, j: int): 'F = this.Values.[i - 1, j - 1]
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

    // Applies a single elementary matrix transformation.
    static member inline ( * ) (op: ElementaryOperation< ^G>, mat: Matrix< ^G>): Matrix< ^G> =
        let m = Array2D.length1 (mat.Values)
        let n = Array2D.length2 (mat.Values)
        match op with
        | SwapRow(a, b) ->
            Matrix< ^G>.Init m n
                    (fun i j -> match i with
                                | row when row = a -> mat.[b, j]
                                | row when row = b -> mat.[a, j]
                                | _ -> mat.[i, j])
        | ScaleRow(row, scalar) ->
            Matrix< ^G>.Init m n
                    (fun i j -> if i = row then mat.[i, j] * scalar
                                else mat.[i, j])
        | AddScaledRow(targetRow, scalar, row) ->
            Matrix< ^G>.Init m n
                    (fun i j -> if i = targetRow then scalar * mat.[row, j] + mat.[i, j]
                                else mat.[i, j])
        | SwapColumn(a, b) ->
            Matrix< ^G>.Init m n
                    (fun i j -> match j with
                                | col when col = a -> mat.[i, b]
                                | col when col = b -> mat.[i, a]
                                | _ -> mat.[i, j])
        | ScaleColumn(column, scalar) ->
            Matrix< ^G>.Init m n
                    (fun i j -> if j = column then scalar * mat.[i, j]
                                else mat.[i, j])
        | AddScaledColumn(targetColumn, scalar, column) ->
            Matrix< ^G>.Init m n
                    (fun i j -> if j = targetColumn then mat.[i, j] + scalar * mat.[i, column]
                                else mat.[i, j])

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

    // Computes the determinant using Leibnitz formula
    let inline determinantLeibnitz (mat: Matrix< ^F>) : ^F =
        let sum (acc: ^F) (pi: Permutation) : ^F =
            let rhs = Map.fold (fun acc i p -> acc * mat.[i, p]) GenericZero (pi.ToMap())
            if Permutation.sgn pi > 0 then
                acc + rhs
            else
                acc - rhs
        if not (isQuadratic mat) then
            invalidArg "mat" "Matrix must be quadratic."
        List.fold sum GenericZero (Permutation.all (rowCount mat))

    /// Computes the determinant of given matrix using Laplace expansion.
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
        let elements =
            seq {
                for i = 1 to rowCount mat do
                    for j = 1 to columnCount mat do
                        yield mat.[i, j]
            }
        Seq.fold folder init elements

    let inline density (mat: Matrix< ^F>): float =
        let total = rowCount mat * columnCount mat
        let zeros = foldElements (fun a v -> if v <> GenericZero then a + 1 else a) 0 mat
        float zeros / float total

    let inline sparsity (mat: Matrix< ^F>): float =
        1.0 - (density mat)

// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch

// open System.Numerics
open System

module Helper =
    let GreatestCommonDivisor (a: bigint) (b: bigint) =
        // Recursive implementation of finding the GCD.
        let rec gcd a b =
            if b = 0I then a
            else gcd b (a % b)
        if a.IsZero || b.IsZero then
            bigint.Zero
        else
            gcd a b

[<StructuredFormatDisplay("{AsString}")>]
//[<StructuredFormatDisplay("{StructuredDisplayString}N")>]
type BigRational(_nominator: bigint, _denominator: bigint) =
    member val Nominator = _nominator
    member val Denominator = _denominator

    new(value: int) = BigRational(bigint value, bigint.One)

    static member op_Explicit (value: BigRational): float =
        float (value.Nominator) / float (value.Denominator)

    static member Zero = BigRational(0I, 1I)
    static member One = BigRational(1I, 1I)
    static member MinusOne = BigRational(-1I, 1I)

    member this.Sign = this.Nominator.Sign

    static member Normalize(a: bigint, b: bigint) =
        if b = bigint.Zero then
            BigRational.Zero
        elif b = bigint.One then
            BigRational(a, b)
        else
            let gcd = bigint.GreatestCommonDivisor (a, b)
            let a = a / gcd
            let b = b / gcd
            if b < bigint.Zero then
                BigRational(-a, -b)
            else
                BigRational(a, b)

    static member Create(a: int, b: int) = BigRational.Normalize (bigint a, bigint b)
    static member Create(a, b) = BigRational.Normalize (a, b)

    static member FromInt(n: int) = BigRational.Normalize (bigint n, bigint.One)
    static member FromIntFraction(a: int, b: int) = BigRational.Normalize (bigint a, bigint b)
    static member FromBigInt(n: bigint) = BigRational.Normalize (n, bigint.One)
    static member FromBigIntFraction(a, b: bigint) = BigRational.Normalize (a, b)

    member this.AsString = this.ToString()

    override this.ToString() =
        if this.Nominator.IsZero || this.Denominator.IsZero then "0"
        elif this.Denominator.IsOne then this.Nominator.ToString()
        else sprintf "%s/%s" (this.Nominator.ToString()) (this.Denominator.ToString())

    static member ( ~+ ) (a: BigRational) =
        BigRational.Normalize (-a.Nominator, a.Denominator)

    static member ( ~- ) (a: BigRational) =
        BigRational.Normalize (-a.Nominator, a.Denominator)

    static member ( + ) (a: BigRational, b: BigRational) =
        BigRational.Normalize (a.Nominator*b.Denominator + b.Nominator*a.Denominator, a.Denominator * b.Denominator)

    static member ( - ) (a: BigRational, b: BigRational) =
        BigRational.Normalize (a.Nominator*b.Denominator - b.Nominator*a.Denominator, a.Denominator * b.Denominator)

    static member ( * ) (a: BigRational, b: BigRational) =
        BigRational.Normalize (a.Nominator * b.Nominator, a.Denominator * b.Denominator)

    static member ( / ) (a: BigRational, b: BigRational) =
        BigRational.Normalize (a.Nominator * b.Denominator,
                               a.Denominator * b.Nominator)

    static member ToDouble (a: BigRational) =
        float a.Nominator / float a.Denominator

    static member Inverse (p: BigRational) =
        BigRational.Normalize (p.Denominator, p.Nominator)

    static member Abs (p: BigRational) =
        BigRational.Normalize (abs(p.Nominator), p.Denominator)

    static member Parse (str: string) =
        match str.Split('/') with
        | [|a|] -> BigRational.FromBigInt(bigint.Parse(a))
        | [|a; b|] -> BigRational.FromBigIntFraction(bigint.Parse(a), bigint.Parse(b))
        | _ -> invalidOp "Invalid string format"

    static member Compare(a: BigRational, b: BigRational) : int =
        compare (a.Nominator * b.Denominator) (b.Nominator * a.Denominator)

    override this.GetHashCode() = hash ( this.Nominator, this.Denominator )

    override this.Equals(thatObj) =
        match thatObj with
        | :? BigRational as that -> this.Nominator = that.Nominator && this.Denominator = that.Denominator
        | _ -> false

    interface System.IComparable with
        member this.CompareTo (obj: obj) : int =
            match obj with
            | :? BigRational as that -> BigRational.Compare(this, that)
            | _ -> invalidArg "obj" "invalid type on right hand side"

    interface System.IComparable<BigRational> with
        member this.CompareTo that =
            BigRational.Compare (this, that)

    // ...

// Provides user defined literal with suffix N.
// Ultimatively, it's not just a rational number, but can also be a natural, complex, real, whatever fits best.
[<RequireQualifiedAccess>]
module NumericLiteralN =
    let private zero = BigRational.Zero
    let private one = BigRational.One
    let FromZero() = zero
    let FromOne() = one
    let FromInt32 x = BigRational.FromInt x
    let FromInt64 (x: int64) = bigint x |> BigRational.FromBigInt
    let FromString (x: string) = BigRational.Parse x


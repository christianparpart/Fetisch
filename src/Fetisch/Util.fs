// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch

module Util =
    open FSharp.Core.LanguagePrimitives

    let inline isZero (v: ^F) =
        v = GenericZero

    let inline isOne (v: ^F) =
        v = GenericOne

    let inline isMinusOne (v: ^F) =
        v < GenericZero && abs v = GenericOne

    let inline isNotZero (v: ^F) =
        v <> GenericZero

    let inline isNotOne (v: ^F) =
        v <> GenericOne

    let inline GreatestCommonDivisor (a: ^F) (b: ^F) : ^F =
        // Recursive implementation of finding the GCD.
        let rec gcd a b =
            if b = GenericZero then a
            else gcd b (a % b)
        if a = GenericZero || b = GenericZero then
            GenericZero
        else
            gcd a b

    let rec roman n =
        // Converts integer to roman letters.
        assert(n > 0)
        let map = Map [(1000, "M");
                       (900, "CM"); (500, "D"); (400, "CD"); (100, "C");
                       (90, "XC"); (50, "L"); (40, "XL"); (10, "X");
                       (9, "IX"); (5, "V"); (4, "IV"); (1, "I")]
        // Returns the greatest key less than or equal to the given key,
        // or null if there is no such key.
        let floorKey key =
            Map.fold (fun acc k _ -> if k <= key && k > acc then k else acc) 0 map
        let l = floorKey n
        if l = n then
            map.[l]
        else
            map.[l] + (roman (n - l))

    let inline additiveIdentity (v: ^F): ^F =
        v - v

    let inline additiveInverse (v: ^F): ^F =
        (additiveIdentity v) - v

    let inline multiplicativeIdentity (v: 'F): 'F =
        if isZero v then
            failwithf "Cannot compute multiplicative identity of %s." (v.ToString())
        v / v

    let inline multiplicativeInverse (v: ^F): ^F =
        (multiplicativeIdentity v) / v


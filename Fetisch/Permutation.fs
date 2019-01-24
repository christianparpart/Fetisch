// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.Algebra

open FSharp.Collections

type Transposition = int * int
type Cycle = int list

type Permutation(values: int []) =
    member val Values = values

    member this.Length = (Array.length this.Values)
    member this.Item with get (n: int) = this.Values.[n]
    member this.Map(n: int) : int =
        if n < this.Values.Length then
            this.[n]
        else
            n

    member this.ToMap() =
        let list = List.init (this.Length) (fun i -> (i, this.[i]))
        let map = Map.ofList list
        let map' = map.Remove(0) // XXX we do not consider the 0 to be part of it
        map'

    member this.ToSimpleCycleForm() = 
        let f acc v =
            let i, s = acc
            let sep = if i = 1 then "" else " "
            match i with
            | 0 -> 1, ""
            | _ -> i + 1, sprintf "%s%s(%d %d)" s sep i v
        let _, s = Array.fold f (0, "") this.Values
        s

    member this.ToCanonicalCycleForm() =
        let largestKey (set: Map<int, int>) : int =
            Map.fold (fun state key _ -> if key > state then key else state) 0 set

        // Builds all cycles it can find in @p pending, starting a new cycle at @p start.
        let rec buildAllCycles (start: int) (pending: Map<int, int>) : Cycle list =
            // Builds a single cycle from pending map
            let rec buildCycle (start: int) (acc: Cycle) (pending: Map<int, int>) : Cycle * Map<int, int> =
                let next = this.[acc.Head]
                let pending' = pending.Remove(next)
                match next = start with
                | true -> acc, pending'
                | false -> buildCycle start (next :: acc) pending'

            let theCycleRev, remaining = buildCycle start [start] pending
            let theCycle = List.rev theCycleRev
            match Map.count remaining with
            | 0 -> [theCycle]
            | _ -> theCycle :: (buildAllCycles (largestKey remaining) remaining)

        // formats a single cycle, such as "(5 1 2)"
        let formatCycle (acc: string) (cycle: Cycle) : string =
            let maybeSep x = if x <> "" then " " else ""
            let fmt (acc: string) (n: int) : string =
                sprintf "%s%s%d" acc (maybeSep acc) n
            sprintf "%s%s(%s)" acc (maybeSep acc) (List.fold fmt "" cycle)

        if this.Length = 0 then
            ""
        else
            let map = this.ToMap()
            List.fold formatCycle "" (buildAllCycles (largestKey map) map)

    override this.ToString() =
        this.ToCanonicalCycleForm()

    // Permutation composition.
    // a * b: x -> b(a(x))
    static member ( * ) (a: Permutation, b: Permutation) : Permutation =
        let compose n =
            a.Map(n)
        let composed = Array.map compose b.Values
        Permutation(composed)

module Permutation =
    let init (n: int) (init: (int -> int)) : Permutation =
        Permutation(Array.init (n+1) (fun i -> if i = 0 then 0 else init i))

    // Creates an identity-permutation with @p n elements (with identity mapping from 1 to n).
    let identity (n: int) : Permutation =
        init n (fun i -> i)

    // Creates an inverse permutation of the input permutation @p p.
    let inverse (p: Permutation) : Permutation =
        Permutation(Array.mapi (fun i _ -> Array.findIndex (fun n' -> n' = i) (p.Values)) (p.Values))

    // Creates a permutation of a list of mappings. The list's head position marks the first input value and its value (1) the permutation.
    let ofList (_values: int list) =
        if List.tryFind (fun v -> v < 1) _values <> None then
            invalidArg "_values" "Invalid input list of permutations. Must not contain elements below 1."

        if List.tryFind (fun v -> v > (List.length _values)) _values <> None then
            invalidArg "_values" "Incomplete permutation specified."

        Permutation(List.toArray (0 :: _values))

    // Creates a Permutation represented as a list of unordered transpositions.
    let ofTranspositions (size: int) (_values: Transposition list) =
        let toArray (values: Transposition list) : int [] =
            let rec tryApply (n: int) (values: Transposition list) : Option<int> = 
                match values with
                | [] -> None
                | (a, b) :: tail ->
                    if a = n then Some b
                    else tryApply n tail
            let apply n values =
                match tryApply n values with
                | Some v -> v
                | None -> n
            Array.init (size + 1) (fun i -> apply i values)
        Permutation(toArray _values)

    let ofTransposition (n: int) (_value: Transposition) =
        let a, b = _value
        let init i =
            if i = a then b
            elif i = b then a
            else i
        Permutation(Array.init n init)

    // Creates a permutation of given @p cycles.
    // All elements of Sn must be present in @p cycles, but not
    // necessarily in order.
    let ofCanonicalCycles (cycles: Cycle list) =
        () // TODO

    // Creates a permutation of @p n elements
    // with given @p cycles.
    let ofCycles (n: int) (cycles: Cycle list) =
        // TODO
        let s : Set<int> = set [ ]
        let flattened = seq {
            for cycle in cycles do
                for o in cycle do
                    yield o
        }
        //Seq.iteri
        Permutation( [| |] )

    let inline length (p: Permutation) : int =
        p.Length

    // Creates a permuted permutation whose elements have
    // been permuted by the @p mapper function.
    let mapn (mapper: (int -> int)) (p: Permutation) : Permutation =
        let permute n =
            let n' = mapper n
            if n' < 1 || n' > p.Values.Length then
                invalidArg "mapper" "Mapper is exceeding the range of the permutation."
            n'
        Permutation(Array.mapi (fun i _ -> permute i) p.Values)

    let failureCount (p: Permutation) : int =
        let f a n =
            let g a i =
                match p.Map(i) > p.Map(n) with
                | true -> a + 1
                | false -> a
            a + Array.fold g 0 [| 1 .. n |]
        Array.fold f 0 [| 1 .. p.Length |]

    let inline isEven (p: Permutation) =
        (failureCount p) % 2 = 0

    let inline isOdd (p: Permutation) =
        (failureCount p) % 2 <> 0

    let inline sgn (m: Permutation) : int =
        match isEven m with
        | true -> +1
        | false -> -1

    // Retrieves the range of the permutation
    let inline range (p: Permutation) : int array =
        p.Values.[1 ..]
        
    // Constructs a list of all permutations between 1 and n.
    let all (n: int) : Permutation list =
        let rec permute (values: int list): int list list =
            match List.length values with
            | 1 ->
                [values]
            | _ ->
                let f (acc: int list list) (value: int): int list list = 
                    List.fold (fun a l -> (value :: l) :: a)
                              acc
                              (permute (List.except (seq {yield value}) values))
                List.fold f [] values
        List.map (fun (v: int list) -> ofList v) (permute [ 1 .. n ])


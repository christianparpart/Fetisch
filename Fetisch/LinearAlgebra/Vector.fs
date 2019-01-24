// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

namespace Fetisch.LinearAlgebra

type Vector<'T when 'T : equality>(_values: 'T []) =
    member val Values : 'T [] = _values
    member this.Dimension = Array.length this.Values
    member this.Item with get (i: int) : 'T = this.Values.[i - 1]

    static member Init n init =
        Vector(Array.init n (fun i -> init (i + 1)))

    static member ( ~+ ) (a: Vector<_>) =
        Vector<'T>.Init a.Dimension (fun i -> a.[i])

    static member ( ~- ) (a: Vector<_>) =
        Vector<'T>.Init a.Dimension (fun i -> -a.[i])

    static member ( + ) (a: Vector<_>, b: Vector<_>) =
        Vector<'T>.Init a.Dimension (fun i -> a.[i] + b.[i])

    static member ( - ) (a: Vector<_>, b: Vector<_>) =
        Vector<'T>.Init a.Dimension (fun i -> a.[i] - b.[i])

    static member ( * ) (a: Vector<_>, b) =
        Vector<'T>.Init a.Dimension (fun i -> a.[i] * b)

    static member ( * ) (a, b: Vector<_>) =
        Vector<'T>.Init b.Dimension (fun i -> a * b.[i])

    override this.ToString() =
        let rec entries i =
            match i with
            | n when n > this.Dimension -> ""
            | 1 -> sprintf "%s%s" (this.[i].ToString()) (entries (i + 1))
            | _ -> sprintf ", %s%s" (this.[i].ToString()) (entries (i + 1))
        sprintf "(%s)" (entries 1)

    override this.GetHashCode() = hash (this.Values)

    override this.Equals(obj: obj) =
        match obj with
        | :? Vector<'T> as that ->
            this.Values = that.Values
        | _ ->
            false

module Vector =
    let init n init = Vector<_>(Array.init n (fun i -> init (i + 1)))
    let dimension (v: Vector<_>) = v.Dimension

    let fold (folder) (state) (v: #Vector<_>) =
        Array.fold folder state (v.Values)

    let foldi (folder) (state) (v: #Vector<_>) =
        let mutable i = 0
        Array.fold (fun a s -> i <- i + 1; folder a i s) state (v.Values)

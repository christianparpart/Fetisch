// This file is part of the "Fetisch" project, http://github.com/christianparpart/Fetisch>
//   (c) 2009-2018 Christian Parpart <christian@parpart.family>
//
// Licensed under the MIT License (the "License"); you may not use this
// file except in compliance with the License. You may obtain a copy of
// the License at: http://opensource.org/licenses/MIT

module Fetisch.ModNum

open System

type ModNum(_value, _mod) =
    member val Value = _value
    member val Mod = _mod

    new(value: int, modulo: int) =
        ModNum(bigint value, bigint modulo)

    static member Zero = ModNum(0I, 1I)
    static member One = ModNum(1I, 1I)

    override this.ToString() = sprintf "%s mod %s" (this.Value.ToString()) (this.Mod.ToString())

    override this.GetHashCode() = hash (this.Value)

    override this.Equals(obj: obj) =
        match obj with
        | :? ModNum as that ->
            this.Value = that.Value
        | _ ->
            false

    interface IComparable<ModNum> with
        member this.CompareTo(that) =
            compare (this.Value, this.Mod) (that.Value, that.Mod)

    static member op_Explicit (that: ModNum): int32 = int32 that.Value
    static member op_Explicit (that: ModNum): int64 = int64 that.Value
    static member op_Explicit (that: ModNum): float = float that.Value

    static member ( + ) (a: ModNum, b: ModNum): ModNum =
        let m = min (a.Mod) (b.Mod)
        let a' = a.Value % m
        let b' = b.Value % m
        ModNum((a' + b') % m, m)

    static member ( * ) (a: ModNum, b: ModNum): ModNum =
        let m = min (a.Mod) (b.Mod)
        let a' = a.Value % m
        let b' = b.Value % m
        ModNum((a' * b') % m, m)

type modnum = ModNum

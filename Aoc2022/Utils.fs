namespace Aoc2022

open System
open System.Collections.Generic

module Seq =
    let private getNext (e: IEnumerator<'t>) =
        if e.MoveNext() then
            e.Current
        else
            failwith "Not enough elements in a sequence"

    let exactly2 (s: seq<'t>) =
        use e = s.GetEnumerator()
        let t = getNext e, getNext e

        if e.MoveNext() then
            failwith "More than two elements in sequence"
        else
            t

module String =
    let split (sep: string) (s: string) =
        s.Split sep
 
    let splitChar (sep: char) (s: string) =
        s.Split sep
        
    let newLine = Environment.NewLine
 
    let ofChars (chars: seq<char>) = String.Concat chars
 
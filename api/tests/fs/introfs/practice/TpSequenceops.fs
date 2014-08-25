namespace Introfs.Practice.Tests
    
open System
open System.Linq
open NUnit.Framework
//open FsCheck

open Introfs.Practice

[<TestFixture>]
module TpSequenceops =
    
    module Util = Introfs.Util.Library
    type PropertyAttribute = FsCheck.NUnit.PropertyAttribute
    type MyArbitraries = Base.MyArbitraries
    
    let (modNm, epsilon) = ("TpSequenceops", 0.001)
    let curry f a b = f(a, b)
    
    [<Property>] [<Category("Tag3")>]
    let ``ReverseProp`` (xs:int list) =
        let (ans, cslst) = (Seq.rev xs, ResizeArray<_>(xs)) in
        Seq.fold (fun acc fn -> acc && ans.SequenceEqual (fn xs)) true
            [Sequenceops.revSeqR; Sequenceops.revSeqI]
        && Seq.fold (fun acc fn ->
            let tmp = SequenceopsCs.CopyOf(cslst) in
            fn(tmp) ; acc && ans.SequenceEqual tmp) true
            [SequenceopsCs.ReverseLp]
    
    [<Property>] [<Category("Tag3")>]
    let ``IndexFindProp`` (el:int) (xs:int list) =
        let pred item = item = el in
        let ans = 
            (*match Seq.tryFindIndex pred xs with
            |   None -> -1
            |   Some x -> x in*)
            Option.defaultValue -1 <| Seq.tryFindIndex pred xs in
        let cslst = ResizeArray<_>(xs) in
        Seq.fold (fun acc fn -> 
            acc && ans = (fn pred xs)) true
            [Sequenceops.findIndexSeqI]
        && Seq.fold (fun acc fn -> 
            acc && ans = (fn el cslst)) true
            [curry SequenceopsCs.IndexOfLp]

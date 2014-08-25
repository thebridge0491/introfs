namespace Introfs.Practice.Tests
    
open System
open NUnit.Framework
open FsUnit

open Introfs.Practice

[<TestFixture>]
module TcSequenceops =
    
    module Util = Introfs.Util.Library
    
    let (modNm, epsilon) = ("TcSequenceops", 0.001)
    let curry f a b = f(a, b)
    
    [<TestFixtureSetUp>]
    let setUpModule () = 
        printf "\nsetUpModule(%s)\n" modNm ; ignore ()
    
    [<TestFixtureTearDown>]
    let tearDownModule () = 
        ignore () ; printf "tearDownModule(%s)\n" modNm
    
    [<SetUp>]
    let setUp () = 
        printf "setUp(%s)\n" modNm ; ignore ()
    
    [<TearDown>]
    let tearDown () = 
        ignore () ; printf "tearDown(%s)\n" modNm
    
    
    let wrapTest startFun endFun testFun =
        startFun () ; testFun () ; endFun ()
    
    let markFunc stage funcNm =
        printf "%s(%s.%s)\n" stage modNm funcNm
    
    [<Test>]
    let ``reverseTest`` () = 
        let lst = [0; 1; 2; 3; 4] in
        let cslst = ResizeArray<_>(lst) in
        (*wrapTest (fun () -> markFunc "SetUp" "reverseTest")
            (fun () -> markFunc "TearDown" "reverseTest")
            (fun () -> 
                Seq.iter (fun fn -> Seq.rev lst |> should equal <| fn lst)
                    [Sequenceops.revSeqR; Sequenceops.revSeqI])*)
        Seq.iter (fun fn -> Seq.rev lst |> should equal <| fn lst)
            [Sequenceops.revSeqR; Sequenceops.revSeqI]
        ; Seq.iter (fun fn ->
            let tmp = SequenceopsCs.CopyOf(cslst) in
            fn(tmp) ; Seq.rev cslst |> should equal tmp)
            [SequenceopsCs.ReverseLp]
    
    [<Test>]
    let ``indexFindTest`` () = 
        let el = 3 in
        let pred item = item = el in
        Seq.iter (fun lst ->
            Seq.iter (fun fn ->
                Seq.findIndex pred lst |> should equal <| fn pred lst)
                [Sequenceops.findIndexSeqI]
            ; Seq.iter (fun fn ->
                let cslst = ResizeArray<_>(lst) in
                Seq.findIndex pred cslst |> should equal <| fn el cslst)
                [curry SequenceopsCs.IndexOfLp])
            [[0 .. 4]; [4 .. -1 .. 0]]

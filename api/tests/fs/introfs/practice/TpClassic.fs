namespace Introfs.Practice.Tests
    
open System
open NUnit.Framework
//open FsCheck

open Introfs.Practice

[<TestFixture>]
module TpClassic =
    
    module Util = Introfs.Util.Library
    type PropertyAttribute = FsCheck.NUnit.PropertyAttribute
    
    let (modNm, epsilon) = ("TpClassic", 0.001)
    let curry f a b = f(a, b)
    
    (*[<TestFixtureSetUp>]
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
        ignore () ; printf "tearDown(%s)\n" modNm*)
    
    [<Property(MaxTest = 20, Verbose = true)>] [<Category("Tag3")>]
    let ``factProp`` (x: uint32) =
        let n = (int64 x) % 19L + 1L in
        let ans = Seq.fold (fun acc e -> acc * e) 1L [1L .. n] in
        Seq.fold (fun acc fn -> acc && ans.Equals (fn n)) true
            [Classic.factR; Classic.factI; ClassicCs.FactLp; ClassicCs.FactI]
    
    [<Property>] [<Category("Tag3")>]
    let ``exptProp`` (x: uint32) (y: uint32) =
        let (b, n) = (float32 <| (int x) % 19 + 1, float32 <| (int y) % 10 + 1) in
        let ans = float <| b ** n in
        Seq.fold (fun acc fn -> 
            acc && (Util.inEpsilon (epsilon * ans) ans (float <| fn b n))) true
            [Classic.exptR; Classic.exptI; curry ClassicCs.ExptLp
                ; curry ClassicCs.ExptI]

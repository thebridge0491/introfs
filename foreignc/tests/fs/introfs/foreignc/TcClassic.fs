namespace Introfs.Foreignc.Tests
    
open System
open NUnit.Framework
open FsUnit

open Introfs.Foreignc

[<TestFixture>]
module TcClassic =
    
    module Util = Introfs.Util.Library
    
    let (modNm, epsilon) = ("TcClassic", 0.001)
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
    let ``factTest`` () = 
        (*wrapTest (fun () -> markFunc "setUp" "factTest")
            (fun () -> markFunc "tearDown" "factTest")
            (fun () -> 
                Seq.iter (fun fn ->
                        120L |> should equal <| fn 5L)
                    [Classic.factLp; Classic.factI])*)
        Seq.iter (fun fn ->
                120L |> should equal <| fn 5L)
            [Classic.factLp; Classic.factI]
    
    [<Test>]
    let ``exptTest`` () = 
        Seq.iter (fun (b, n) ->
            let ans = float <| b ** n in
            Seq.iter (fun fn ->
                ans |> should (equalWithin (epsilon * ans)) (float <| fn b n))
                [Classic.exptLp; Classic.exptI])
            (*(Seq.concat <| Seq.map (fun b -> Seq.map (fun n -> (b, n)) [3.0f; 6.0f; 10.0f]) [2.0f; 11.0f; 20.0f])*)
            [for b in [2.0f; 11.0f; 20.0f] do
                for n in [3.0f; 6.0f; 10.0f] -> (b, n)]

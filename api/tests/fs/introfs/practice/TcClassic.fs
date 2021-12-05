namespace Introfs.Practice.Tests

open System
open NUnit.Framework
open FsUnit

open Introfs.Practice

[<TestFixture>]
module TcClassic =

    module Util = Introfs.Util.Library
    module ClassicHi = ClassicHiorder
    module ClassicStrm = ClassicStreams
    
    let (modNm, epsilon) = ("TcClassic", 0.001)
    let curry f a b = f(a, b)

    [<OneTimeSetUp>]
    let setUpModule () =
        printf "\nsetUpModule(%s)\n" modNm ; ignore ()

    [<OneTimeTearDown>]
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
    let ``squareTest`` () = 
        Seq.iter (fun n ->
            let ans = float <| n ** 2.0f in
            Seq.iter (fun fn ->
                ans |> should (equalWithin (epsilon * ans)) <|
                    (float <| fn n))
                [Classic.squareR; Classic.squareI; ClassicHi.squareF
                ; ClassicHi.squareU; ClassicHi.squareLc]
            ; Seq.iter (fun fn ->
                ans |> should (equalWithin (epsilon * ans)) <|
                    (float <| Seq.item (int n) (fn ())))
                [ClassicStrm.squaresMap2; ClassicStrm.squaresU
                ; ClassicStrm.squaresLc; ClassicStrm.squaresScanl]
            ) [2.0f; 11.0f; 20.0f]
    
    [<Test>]
    let ``exptTest`` () =
        Seq.iter (fun (b, n) ->
            let ans = float <| b ** n in
            Seq.iter (fun fn ->
                ans |> should (equalWithin (epsilon * ans)) (float <| fn b n))
                [Classic.exptR; Classic.exptI; Classic.fastExptR
                    ; Classic.fastExptI; ClassicHi.exptF; ClassicHi.exptU
                    ; ClassicHi.exptLc; curry ClassicCs.ExptLp
                    ; curry ClassicCs.ExptI]
            ; Seq.iter (fun fn ->
                ans |> should (equalWithin (epsilon * ans)) <|
                    (float <| Seq.item (int n) (fn b)))
                [ClassicStrm.exptsMap2; ClassicStrm.exptsU
                ; ClassicStrm.exptsLc; ClassicStrm.exptsScanl])
            (*(Seq.concat <| Seq.map (fun b -> Seq.map (fun n -> (b, n)) [3.0f; 6.0f; 10.0f]) [2.0f; 11.0f; 20.0f])*)
            [for b in [2.0f; 11.0f; 20.0f] do
                for n in [3.0f; 6.0f; 10.0f] -> (b, n)]

    [<Test>]
    let ``sumToTest`` () = 
        Seq.iter (fun (hi, lo) ->
            let ans = Seq.fold (+) 0L [lo .. hi] in
            Seq.iter (fun fn -> 
                ans |> should equal <| fn hi lo)
                [Classic.sumToR; Classic.sumToI; ClassicHi.sumToF
                ; ClassicHi.sumToU; ClassicHi.sumToLc]
            ; Seq.iter (fun fn ->
                ans |> should equal <| Seq.item (int (hi - lo)) (fn lo))
                [ClassicStrm.sumsMap2; ClassicStrm.sumsU
                ; ClassicStrm.sumsLc; ClassicStrm.sumsScanl]
            ) [(5L, 0L); (15L, 10L)]
    
    [<Test>]
    let ``factTest`` () =
        (*wrapTest (fun () -> markFunc "setUp" "factTest")
            (fun () -> markFunc "tearDown" "factTest")
            (fun () ->
                Seq.iter (fun fn ->
                        120L |> should equal <| fn 5L)
                    [Classic.factLp; Classic.factI])*)
        Seq.iter (fun n ->
            let ans = Seq.fold (fun a e -> a * e) 1L [1L .. n] in
            Seq.iter (fun fn -> ans |> should equal <| fn n)
                [ClassicCs.FactLp; ClassicCs.FactI; Classic.factR
                ; Classic.factI; ClassicHi.factF; ClassicHi.factU
                ; ClassicHi.factLc]
            ; Seq.iter (fun fn ->
                ans |> should equal <| Seq.item (int n) (fn ()))
                [ClassicStrm.factsMap2; ClassicStrm.factsU
                ; ClassicStrm.factsLc; ClassicStrm.factsScanl]
            ) [0L; 9L; 18L]
    
    [<Test>]
    let ``fibTest`` () = 
        Seq.iter (fun (n, ans) ->
            Seq.iter (fun fn ->
                ans |> should equal <| fn n)
                [Classic.fibR; Classic.fibI; ClassicHi.fibF
                ; ClassicHi.fibU; ClassicHi.fibLc]
            ; Seq.iter (fun fn ->
                ans |> should equal <| Seq.item (int n) (fn ()))
                [ClassicStrm.fibsMap2; ClassicStrm.fibsU
                ; ClassicStrm.fibsLc; ClassicStrm.fibsScanl]
            ) [(7, 13); (8, 21); (9, 34)]
    
    [<Test>]
    let ``pascaltriTest`` () =
        let ans = [[1]; [1; 1]; [1; 2; 1]; [1; 3; 3; 1]
            ; [1; 4; 6; 4; 1]; [1; 5; 10; 10; 5; 1]] in
        Seq.iter (fun fn ->
            ans |> should equal <| fn 5)
            [Classic.pascaltriMult; Classic.pascaltriAdd
            ; ClassicHi.pascaltriF; ClassicHi.pascaltriU
            ; ClassicHi.pascaltriLc]
        ; Seq.iter (fun fn ->
            ans |> should equal <| Seq.take (5 + 1) (fn ()))
            [ClassicStrm.pascalrowsMap2; ClassicStrm.pascalrowsU
            ; ClassicStrm.pascalrowsLc; ClassicStrm.pascalrowsScanl]
    
    [<Test>]
    let ``quotRemTest`` () =
        Seq.iter (fun (a, b) ->
            (a / b) |> should equal (Classic.quotM a b)
            ; (a % b) |> should equal (Classic.remM a b))
            (Seq.concat <| Seq.map (fun a ->
                Seq.map (fun b -> (a, b)) [3; -3]) [10; -10])

    [<Test>]
    let ``divModTest`` () =
        Seq.iter (fun (a, b) ->
            let (ansD, ansM) = (float <| a / b, float <| a % b) in
            ansD |> should (equalWithin (epsilon * (abs ansD)))
                (float <| Classic.divM a b)
            ; ansM |> should (equalWithin (epsilon * (abs ansM)))
                (float <| Classic.modM a b))
            (Seq.concat <| Seq.map (fun a ->
                Seq.map (fun b -> (a, b)) [3.0f; -3.0f]) [10.0f; -10.0f])

    [<Test>]
    let ``gcdLcmTest`` () =
        Seq.iter (fun (fnGcd, fnLcm) ->
            8 |> should equal <| fnGcd [24; 16]
            ; 4 |> should equal <| fnGcd [24; 16; 12]
            ; 48 |> should equal <| fnLcm [24; 16]
            ; 96 |> should equal <| fnLcm [24; 16; 32])
            [(Classic.gcdR, Classic.lcmR); (Classic.gcdI, Classic.lcmI)
            ; (ClassicHi.gcdF, ClassicHi.lcmF)
            ; (ClassicHi.gcdU, ClassicHi.lcmU)]
    
    [<Test>]
    let ``baseExpandTest`` () =
        Seq.iter (fun fn ->
            [1; 0; 1; 1] |> should equal <| fn 2 11
            ; [1; 1; 0; 1] |> should equal <| fn 4 81)
            [Classic.baseExpandR; Classic.baseExpandI
            ; ClassicHi.baseExpandF; ClassicHi.baseExpandU
            ; ClassicHi.baseExpandLc]
    
    [<Test>]
    let ``baseTo10Test`` () =
        Seq.iter (fun fn ->
            11 |> should equal <| fn 2 [1; 0; 1; 1]
            ; 81 |> should equal <| fn 4 [1; 1; 0; 1])
            [Classic.baseTo10R; Classic.baseTo10I; ClassicHi.baseTo10F
            ; ClassicHi.baseTo10U; ClassicHi.baseTo10Lc]
    
    [<Test>]
    let ``rangeTest`` () =
        let (lst, revlst) = ([0 .. 4], [4 .. -1 .. 0 ]) in
        Seq.iter (fun (fnStep, fnRg) ->
            lst |> should equal <| fnRg 0 4
            ; revlst |> should equal <| fnStep -1 4 0)
            [(Classic.rangeStepR, Classic.rangeR)
            ; (Classic.rangeStepI, Classic.rangeI)
            ; (ClassicHi.rangeStepF, ClassicHi.rangeF)
            ; (ClassicHi.rangeStepU, ClassicHi.rangeU)]
    
    [<Test>]
    let ``composeTest`` () =
        2.0 |> should (equalWithin (epsilon * 2.0)) <|
            Classic.compose1 (fun x -> x ** 2.0) sqrt 2.0
        ; 5 |> should equal <| Classic.compose1 List.length
            (fun n -> [0 .. (n - 1)]) 5

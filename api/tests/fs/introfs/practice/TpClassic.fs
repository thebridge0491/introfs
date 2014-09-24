namespace Introfs.Practice.Tests
    
open System
open NUnit.Framework
//open FsCheck

open Introfs.Practice

[<TestFixture>]
module TpClassic =
    
    module Util = Introfs.Util.Library
    module ClassicHi = ClassicHiorder
    module ClassicStrm = ClassicStreams
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
    
    [<Property>]
    let ``squareProp`` (x:uint32) =
        let n = float32 <| (int x) % 19 + 1 in
        let ans = float <| n ** 2.0f in
        Seq.fold (fun acc fn ->
            acc && (Util.inEpsilon (epsilon * ans) ans (float <| fn n))) true
            [Classic.squareR; Classic.squareI; ClassicHi.squareF
                ; ClassicHi.squareU; ClassicHi.squareLc]
        && Seq.fold (fun acc fn -> 
            acc && (Util.inEpsilon (epsilon * ans) ans
                (float <| Seq.item (int n) (fn ())))) true
            [ClassicStrm.squaresMap2; ClassicStrm.squaresU
            ; ClassicStrm.squaresLc; ClassicStrm.squaresScanl]
    
    [<Property>] [<Category("Tag3")>]
    let ``exptProp`` (x: uint32) (y: uint32) =
        let (b, n) = (float32 <| (int x) % 19 + 1, float32 <| (int y) % 10 + 1) in
        let ans = float <| b ** n in
        Seq.fold (fun acc fn -> 
            acc && (Util.inEpsilon (epsilon * ans) ans (float <| fn b n))) true
            [Classic.exptR; Classic.exptI; Classic.fastExptR
                ; Classic.fastExptI; ClassicHi.exptF; ClassicHi.exptU
                ; ClassicHi.exptLc; curry ClassicCs.ExptLp
                ; curry ClassicCs.ExptI]
        && Seq.fold (fun acc fn -> 
            acc && (Util.inEpsilon (epsilon * ans) ans
                (float <| Seq.item (int n) (fn b)))) true
            [ClassicStrm.exptsMap2; ClassicStrm.exptsU
            ; ClassicStrm.exptsLc; ClassicStrm.exptsScanl]
    
    [<Property>]
    let ``sumToProp`` (x:int64) (y:int64) =
        let (hi, lo) = match x >= y with | true -> (x, y) | _ -> (y, x) in
        let ans = Seq.fold (fun acc e -> acc + e) lo [(lo + 1L) .. hi] in
        Seq.fold (fun acc fn ->
            acc && ans.Equals (fn hi lo)) true
            [Classic.sumToR; Classic.sumToI; ClassicHi.sumToF
            ; ClassicHi.sumToU; ClassicHi.sumToLc]
        && Seq.fold (fun acc fn -> 
            acc && ans = (Seq.item (int (hi - lo)) (fn lo))) true
            [ClassicStrm.sumsMap2; ClassicStrm.sumsU
            ; ClassicStrm.sumsLc; ClassicStrm.sumsScanl]
    
    [<Property(MaxTest = 20, Verbose = true)>] [<Category("Tag3")>]
    let ``factProp`` (x: uint32) =
        let n = (int64 x) % 19L + 1L in
        let ans = Seq.fold (fun acc e -> acc * e) 1L [1L .. n] in
        Seq.fold (fun acc fn -> acc && ans.Equals (fn n)) true
            [Classic.factR; Classic.factI; ClassicHi.factF; ClassicHi.factU
            ; ClassicHi.factLc; ClassicCs.FactLp; ClassicCs.FactI]
        && Seq.fold (fun acc fn -> 
            acc && ans = (Seq.item (int n) (fn ()))) true
            [ClassicStrm.factsMap2; ClassicStrm.factsU
            ; ClassicStrm.factsLc; ClassicStrm.factsScanl]
    
    [<Property>]
    let ``fibProp`` (x:uint32) =
        let n = (int x) % 19 + 1 in
        let ans = snd (Seq.fold (fun (s0, s1) _ -> (s0 + s1, s0)) (0, 1) 
            [0 .. n]) in
        Seq.fold (fun acc fn -> acc && ans.Equals (fn n)) true
            [Classic.fibR; Classic.fibI; ClassicHi.fibF; ClassicHi.fibU
            ; ClassicHi.fibLc]
        && Seq.fold (fun acc fn -> 
            acc && ans = (Seq.item (int n) (fn ()))) true
            [ClassicStrm.fibsMap2; ClassicStrm.fibsU
            ; ClassicStrm.fibsLc; ClassicStrm.fibsScanl]
    
    [<Property>]
    let ``pascaltriProp`` (x:uint32) =
        let rows = (int x) % 15 + 1 in
        let validNumRows res = (Seq.length res) = (rows + 1) in
        let validLenRow n r = (Seq.length r) = (n + 1) in
        let validSumRow n r =
            (Seq.fold (+) 0 r) = (int (2.0 ** (float n))) in
        Seq.fold (fun a f -> a && validNumRows (f rows) &&
            fst (Seq.fold (fun (a1, n) r -> (a1 && validLenRow n r
            && validSumRow n r, n + 1)) (true, 0) (f rows))) true
            [Classic.pascaltriMult; Classic.pascaltriAdd
            ; ClassicHi.pascaltriF; ClassicHi.pascaltriU
            ; ClassicHi.pascaltriLc]
        && Seq.fold (fun a fn -> 
            let res = Seq.take (rows + 1) (fn ()) in
            a && validNumRows res &&
            fst (Seq.fold (fun (a1, n) r -> (a1 && validLenRow n r
            && validSumRow n r, n + 1)) (true, 0) res)) true
            [ClassicStrm.pascalrowsMap2; ClassicStrm.pascalrowsU
            ; ClassicStrm.pascalrowsLc; ClassicStrm.pascalrowsScanl]
    
    [<Property>]
    let ``quotRemProp`` (x:int32) (y:uint32) =
        let (a, b) = ((int x) % 100 + 1, (int y) % 100 + 1) in
        let (ansQ, ansR) = (a / b, a % b) in
        Seq.fold (fun acc (fnQ, fnR) ->
            acc && (fnQ a b) = ansQ && (fnR a b) = ansR) true
            [(Classic.quotM, Classic.remM)]
    
    [<Property>]
    let ``divModProp`` (x:int32) (y:uint32) =
        let (a, b) = (float32 <| (int x) % 100 + 1,
            float32 <| (int y) % 100 + 1) in
        let (ansD, ansM) = (float <| a / b, float <| a % b) in
        Seq.fold (fun acc (fnD, fnM) ->
            acc && (Util.inEpsilon (epsilon * ansD) ansD (float <| fnD a b))
            && (Util.inEpsilon (epsilon * ansM) ansM (float <| fnM a b))) true
            [(Classic.divM, Classic.modM)]
    (*
    [<Property>]
    let ``gcdLcmProp`` (mss:int32 list) =
        match mss with
        |   m::ms ->
                let (ansG, ansL) = (List.fold (fun a b -> 
                    Classic.euclidI a b) m ms,
                    List.fold (fun a b -> a * b / (
                        Classic.euclidI a b)) m ms) in
                List.fold (fun acc (fnG, fnL) -> acc && ansG.Equals (fnG mss) && ansL.Equals (fnL mss)) true
                    [(Classic.gcdR, Classic.lcmR)
                    ; (Classic.gcdI, Classic.lcmI)
                    ; (ClassicHi.gcdF, ClassicHi.lcmF)
                    ; (ClassicHi.gcdU, ClassicHi.lcmU)]
        |   _ -> true
    *)
    
    [<Property>]
    let ``baseExpandProp`` (x:uint32) (y:uint32) =
        let (b, n) = ((int x) % 11 + 2, (int y) % 250 + 1) in
        let ans = fst (List.fold (fun (acc, num) _ ->
                match num with
                | 0 -> (acc, num / b)
                | _ -> (num % b :: acc, num / b))
                ([], n) [0 .. (int (log (float n) / log (float b)))]) in
        Seq.fold (fun acc fn ->
            acc && ans.Equals (fn b n)) true
            [Classic.baseExpandR; Classic.baseExpandI
            ; ClassicHi.baseExpandF; ClassicHi.baseExpandU
            ; ClassicHi.baseExpandLc]
    
    [<Property>]
    let ``baseTo10Prop`` (nss:uint32 list) =
        let nssBs2 = List.truncate 7 <| List.map (fun e -> int <| e % 2u) nss in
        let nssBs16 = List.truncate 7 <| List.map (fun e -> int <| e % 16u) nss in
        let ansBs2 = snd (List.foldBack (fun e (h, t) ->
            (h + 1, t + (e * (int (2.0 ** float h))))) nssBs2 (0, 0)) in
        let ansBs16 = snd (List.foldBack (fun e (h, t) ->
            (h + 1, t + (e * (int (16.0 ** float h))))) nssBs16 (0, 0)) in
        Seq.fold (fun acc fn ->
            acc && ansBs2.Equals (fn 2 nssBs2)
            && ansBs16.Equals (fn 16 nssBs16)) true
            [Classic.baseTo10R; Classic.baseTo10I; ClassicHi.baseTo10F
            ; ClassicHi.baseTo10U; ClassicHi.baseTo10Lc]
    
    [<Property>]
    let ``rangeProp`` (start:int32) (stop:int32) =
        let ans =
            match stop >= start with
            |   true -> [start .. stop]
            |   _ -> [] in
        Seq.fold (fun acc (fnStep, fnRg) ->
            acc && ans.Equals (fnRg start stop)
            && ans.Equals (fnStep 1 start stop)) true
            [(Classic.rangeStepR, Classic.rangeR)
            ; (Classic.rangeStepI, Classic.rangeI)
            ; (ClassicHi.rangeStepF, ClassicHi.rangeF)
            ; (ClassicHi.rangeStepU, ClassicHi.rangeU)]
    
    [<Property>]
    let ``composeProp`` (numI:int32) =
        let numF = float numI in
        let ansI = List.length <| (fun n -> [0 .. (n - 1)]) numI in
        let ansF = (fun n -> n ** 2.0) <| sqrt numF in
        ansI.Equals (Classic.compose1 List.length (fun n -> 
            [0 .. (n - 1)]) numI) && 
            ansF.Equals (Classic.compose1 (fun n -> n ** 2.0) sqrt numF)

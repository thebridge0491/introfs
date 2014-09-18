namespace Introfs.Practice.Tests
    
open System
open System.Linq
open NUnit.Framework
//open FsCheck

open Introfs.Practice

[<TestFixture>]
module TpSequenceops =
    
    module Util = Introfs.Util.Library
    module Seqops = Sequenceops
    module ListopsHi = ListopsHiorder
    type PropertyAttribute = FsCheck.NUnit.PropertyAttribute
    type MyArbitraries = Base.MyArbitraries
    
    let (modNm, epsilon) = ("TpSequenceops", 0.001)
    let curry f a b = f(a, b)
    
    [<Property>]
    let ``tabulateProp`` (x:uint32) =
        let n = (int x) % 19 + 1 in
        let ans = List.init n id in
        Seq.fold (fun acc fn -> acc && ans.SequenceEqual (fn id n)) true
            [Seqops.tabulateR; Seqops.tabulateI]
        && Seq.fold (fun acc fn -> acc && ans.SequenceEqual (fn id n)) true
            [Listops.tabulateR; Listops.tabulateI; ListopsHi.tabulateF
            ; ListopsHi.tabulateU; ListopsHi.tabulateLc]
    
    [<Property>]
    let ``lengthProp`` (xs:int list) =
        let lst = List.truncate 15 xs in
        let ans = Seq.length lst in
        Seq.fold (fun acc fn -> acc && ans = (fn lst)) true
            [Seqops.lengthR; Seqops.lengthI]
        && Seq.fold (fun acc fn -> acc && ans = (fn lst)) true
            [Listops.lengthR; Listops.lengthI; ListopsHi.lengthF
            ; ListopsHi.lengthU]
    
    [<Property>]
    let ``nthProp`` (n:uint32) (xs:int list) =
        let (idx, lst) = ((int n) % 20, List.truncate 20 xs) in
        let ans = 
            match idx < (Seq.length xs) with
            | true -> Some (Seq.item idx lst) | _ -> None in
        Seq.fold (fun acc fn -> acc && ans = (fn idx lst)) true
            [Seqops.nthR; Seqops.nthI]
        && Seq.fold (fun acc fn -> acc && ans = (fn idx lst)) true
            [Listops.nthR; Listops.nthI; ListopsHi.nthF; ListopsHi.nthU
            ; ListopsHi.nthLc]
    
    [<Property>] [<Category("Tag3")>]
    let ``indexFindProp`` (el:int) (xs:int list) =
        let pred item = item = el in
        let (ansI, ansF) = (Seq.tryFindIndex pred xs, Seq.tryFind pred xs) in
        let cslst = ResizeArray<_>(xs) in
        Seq.fold (fun acc fn -> 
            acc && (Option.defaultValue -1 ansI) = (fn el cslst)) true
            [curry SequenceopsCs.IndexOfLp]
        && Seq.fold (fun acc (fnI, fnF) -> 
            acc && ansI = (fnI pred xs) && ansF = (fnF pred xs)) true
            [(Seqops.findIndexR, Seqops.findR)
                ; (Seqops.findIndexI, Seqops.findI)
                ; (Listops.findIndexR, Listops.findR)
                ; (Listops.findIndexI, Listops.findI)
                ; (ListopsHi.findIndexF, ListopsHi.findF)
                ; (ListopsHi.findIndexU, ListopsHi.findU)
                ; (ListopsHi.findIndexLc, ListopsHi.findLc)]
    
    [<Property>]
    let ``minMaxProp`` (xs:int list) =
        let lst = match xs with | [] -> [-1] | _ -> List.truncate 20 xs in
        let (ansMin, ansMax) = (Seq.min lst, Seq.max lst) in
        Seq.fold (fun acc (fnMin, fnMax) -> acc && (ansMin = (fnMin lst)) &&
            (ansMax = (fnMax lst))) true
            [(Seqops.minR, Seqops.maxR); (Seqops.minI, Seqops.maxI)
            ; (Listops.minR, Listops.maxR); (Listops.minI, Listops.maxI)
            ; (ListopsHi.minF, ListopsHi.maxF)
            ; (ListopsHi.minU, ListopsHi.maxU)]
    
    [<Property>] [<Category("Tag3")>]
    let ``reverseProp`` (xs:int list) =
        let (ans, cslst) = (Seq.rev xs, ResizeArray<_>(xs)) in
        Seq.fold (fun acc fn ->
            let tmp = SequenceopsCs.CopyOf(cslst) in
            fn(tmp) ; acc && ans.SequenceEqual tmp) true
            [SequenceopsCs.ReverseLp]
        && Seq.fold (fun acc fn -> acc && ans.SequenceEqual (fn xs)) true
            [Seqops.revR; Seqops.revI]
        && Seq.fold (fun acc fn -> acc && ans.SequenceEqual (fn xs)) true
            [Listops.revR; Listops.revI; ListopsHi.revF; ListopsHi.revU]
    
    [<Property>]
    let ``copyProp`` (xs:int list) =
        let (ans, cslst) = (Seq.map id xs, ResizeArray<_>(xs)) in
        Seq.fold (fun acc fn -> acc && ans.SequenceEqual (fn cslst)) true
            [Seqops.copyR; Seqops.copyI]
        && Seq.fold (fun acc fn -> acc && ans.SequenceEqual (fn xs)) true
            [Seqops.copyR; Seqops.copyI]
        && Seq.fold (fun acc fn -> acc && ans.SequenceEqual (fn xs)) true
            [Listops.copyR; Listops.copyI; ListopsHi.copyF; ListopsHi.copyU
            ; ListopsHi.copyLc]
    
    [<Property>]
    let ``takeDropProp`` (num:uint32) (xs:int list) =
        let lst = List.truncate 20 (xs @ [-1]) in
        let n = (int num) % (Seq.length lst) in
        let (ansT, ansD) = (List.take n lst, List.skip n lst) in
        Seq.fold (fun acc (fnT, fnD) ->
            acc && ansT.SequenceEqual <| (fnT n lst) && 
            ansD.SequenceEqual (fnD n lst)) true
            [(Seqops.takeI, Seqops.dropI)]
        && Seq.fold (fun acc (fnT, fnD) ->
            acc && ansT.SequenceEqual <| (fnT n lst) && 
            ansD.SequenceEqual (fnD n lst)) true
            [(Listops.takeI, Listops.dropI)
            ; (ListopsHi.takeF, ListopsHi.dropF)
            ; (ListopsHi.takeU, ListopsHi.dropU)
            ; (ListopsHi.takeLc, ListopsHi.dropLc)]
    
    [<Property>]
    let ``existsForallProp`` (xs:int list) =
        let lst = List.truncate 20 (xs @ [-1]) in
        let pred1 = (fun el -> 0 = (el % 2)) in
        let (ansE, ansA) = (Seq.exists pred1 lst, Seq.forall pred1 lst) in
        Seq.fold (fun acc (fnE, fnA) ->
            acc && (ansE = (fnE pred1 lst)) &&
            (ansA = (fnA pred1 lst))) true
            [(Seqops.existsR, Seqops.forallR)
            ; (Seqops.existsI, Seqops.forallI)
            ; (Listops.existsR, Listops.forallR)
            ; (Listops.existsI, Listops.forallI)
            ; (ListopsHi.existsF, ListopsHi.forallF)
            ; (ListopsHi.existsU, ListopsHi.forallU)]
    
    [<Property>]
    let ``mapProp`` (xs:int list) =
        let lst = List.truncate 20 (xs @ [-1]) in
        let proc = (fun el -> el + 2) in
        let ans = List.map proc lst in
        Seq.fold (fun acc fn ->
            acc && ans.SequenceEqual (fn proc lst)) true
            [Seqops.mapR; Seqops.mapI]
        && Seq.fold (fun acc fn ->
            acc && ans.SequenceEqual (fn proc lst)) true
            [Listops.mapR; Listops.mapI; ListopsHi.mapF; ListopsHi.mapU
            ; ListopsHi.mapLc]
    
    [<Property>]
    let ``iterProp`` (xs:int list) =
        let lst = List.truncate 20 (xs @ [-1]) in
        let proc = (fun el -> Printf.printf "%d " el) in
        let ans = Seq.iter proc lst in
        Seq.fold (fun acc fn ->
            acc && ans = (fn proc lst)) true
            [Seqops.iterR; Seqops.iterI; Listops.iterR; Listops.iterI
            ; ListopsHi.iterF; ListopsHi.iterU; ListopsHi.iterLc]
    
    [<Property>]
    let ``filterRemoveProp`` (xs:int list) =
        let lst = List.truncate 20 (xs @ [-1]) in
        let pred1 = (fun el -> 0 = (el % 2)) in
        let (ansF, ansR) = (List.filter pred1 lst,
            List.filter (pred1 >> not) lst) in
        Seq.fold (fun acc (fnF, fnR) ->
            acc && ansF.SequenceEqual <| (fnF pred1 lst) && 
            ansR.SequenceEqual (fnR pred1 lst)) true
            [(Seqops.filterR, Seqops.removeR)
            ; (Seqops.filterI, Seqops.removeI)]
        && Seq.fold (fun acc (fnF, fnR) ->
            acc && ansF.SequenceEqual <| (fnF pred1 lst) && 
            ansR.SequenceEqual (fnR pred1 lst)) true
            [(Listops.filterR, Listops.removeR)
            ; (Listops.filterI, Listops.removeI)
            ; (ListopsHi.filterF, ListopsHi.removeF)
            ; (ListopsHi.filterU, ListopsHi.removeU)
            ; (ListopsHi.filterLc, ListopsHi.removeLc)]
    
    [<Property>]
    let ``foldlFoldrProp`` (xs:int list) =
        let lst = List.truncate 20 (xs @ [-1]) in
        let (corp1, proc1) = ((+), (-)) in
        let (ansL, ansR) = (Seq.fold corp1 0 lst, Seq.foldBack proc1 lst 0) in
        Seq.fold (fun acc (fnL, fnR) -> 
            acc && (ansL = (fnL corp1 0 lst)) &&
            (ansR = (fnR proc1 lst 0))) true
            [(Seqops.foldLeftR, Seqops.foldRightR)
            ; (Seqops.foldLeftI, Seqops.foldRightI)
            ; (Listops.foldLeftR, Listops.foldRightR)
            ; (Listops.foldLeftI, Listops.foldRightI)]
    
    [<Property>]
    let ``unfoldrUnfoldlProp`` (x:uint32) =
        let n = (int x) % 21 + 1 in
        let funcRRg (start, stop) =
            match stop < start with
            | true -> None | _ -> Some (start, (start + 1, stop)) in
        let funcRbsexpand (b, num) =
            match num <= 0 with
            | true -> None | _ -> Some (num % b, (b, num / b)) in
        let funcLfib (s0, s1, num) =
            match num < 0 with
            | true -> None | _ -> Some (s0, (s0 + s1, s0, num - 1)) in
        let funcLunsum (start, stop) =
            match stop < start with
            | true -> None | _ -> Some (start, (start + 1, stop - start)) in
        let ansRRg = Seq.unfold funcRRg (0, n) in
        let ansRbs2expand = Seq.unfold funcRbsexpand (2, n) in
        let ansLfib = Seq.unfold funcLfib (0, 1, n) in
        let ansLunsum = Seq.unfold funcLunsum (0, n) in
        Seq.fold (fun acc (fnR, fnL) ->
            acc && ansLfib.SequenceEqual (fnL funcLfib (0, 1, n))) true
            [(Seqops.unfoldRightI, Seqops.unfoldLeftR)]
        && Seq.fold (fun acc (fnR, fnL) ->
            acc && ansRRg.SequenceEqual (Seq.rev <| fnR funcRRg (0, n)) && 
            ansRbs2expand.SequenceEqual (Seq.rev <|
                fnR funcRbsexpand (2, n)) && 
            ansLunsum.SequenceEqual (fnL funcLunsum (0, n))) true
            [(Seqops.unfoldRightI, Seqops.unfoldLeftR)]
        && Seq.fold (fun acc (fnR, fnL) ->
            acc && ansLfib.SequenceEqual (fnL funcLfib (0, 1, n))) true
            [(Listops.unfoldRightI, Listops.unfoldLeftR)]
        && Seq.fold (fun acc (fnR, fnL) ->
            acc && ansRRg.SequenceEqual (Seq.rev <| fnR funcRRg (0, n)) && 
            ansRbs2expand.SequenceEqual (Seq.rev <|
                fnR funcRbsexpand (2, n)) && 
            ansLunsum.SequenceEqual (fnL funcLunsum (0, n))) true
            [(Listops.unfoldRightI, Listops.unfoldLeftR)]
    
    [<Property>]
    let ``isOrderedProp`` (xs:int list) =
        let lst = List.truncate 20 xs in
        let verifyOrder cmpfn keyfn lst =
            match Seq.isEmpty lst with
            | true -> true
            | _ ->
                let (x, xs) = (Seq.head lst, Seq.tail lst) in
                fst <| Seq.fold (fun (a, cur) e ->
                    ((cmpfn cur (keyfn e)) && a, (keyfn e))) (true, x) lst in
        let ansOrd = verifyOrder (<=) id lst in
        let ansRev = verifyOrder (>=) id lst in
        Seq.fold (fun acc fn ->
            acc && ansOrd = (fn (<=) id lst) &&
            ansRev = (fn (>=) id lst)) true
            [Seqops.isOrderedR; Seqops.isOrderedI; Listops.isOrderedR
            ; Listops.isOrderedI; ListopsHi.isOrderedF; ListopsHi.isOrderedU
            ; ListopsHi.isOrderedLc]
    
    
    [<Property>]
    let ``appendProp`` (xs:int list) (ys:int list) =
        let (wss, zss) = (List.truncate 20 xs, List.truncate 20 ys) in
        let ans = List.append wss zss in
        Seq.fold (fun acc fn ->
            acc && ans.SequenceEqual (fn wss zss)) true
            [Seqops.appendR; Seqops.appendI]
        && Seq.fold (fun acc fn ->
            acc && ans.SequenceEqual (fn wss zss)) true
            [Listops.appendR; Listops.appendI; ListopsHi.appendF
            ; ListopsHi.appendU]
    
    [<Property>]
    let ``interleaveProp`` (xs:int list) (ys:int list) =
        let (wss, zss) = (List.truncate 20 xs, List.truncate 20 ys) in
        let lenShort =
            match Seq.length wss < Seq.length zss with
            | true -> Seq.length wss | _ -> Seq.length zss in
        let ans = fst <| Seq.foldBack (fun e (acc, rst) ->
            let (z, zs) = (Seq.head rst, Seq.tail rst) in
            (Seq.append [z] <| Seq.append [e] acc, zs)) (Seq.take lenShort zss) (Seq.append (Seq.skip lenShort wss) (Seq.skip lenShort zss), Seq.rev <| Seq.take lenShort wss) in
        Seq.fold (fun acc fn ->
            acc && ans.SequenceEqual (fn wss zss)) true
            [Seqops.interleaveR; Seqops.interleaveI]
        && Seq.fold (fun acc fn ->
            acc && ans.SequenceEqual (fn wss zss)) true
            [Listops.interleaveR; Listops.interleaveI; ListopsHi.interleaveF
            ; ListopsHi.interleaveU; ListopsHi.interleaveLc]
    
    [<Property>]
    let ``map2Prop`` (xs:int list) (ys:int list) =
        let (wss, zss) = (List.truncate 20 xs, List.truncate 20 ys) in
        let lenShort =
            match Seq.length wss < Seq.length zss with
            | true -> Seq.length wss | _ -> Seq.length zss in
        let proc = (fun e1 e2 -> e1 + e2 + 2) in
        let ans = Seq.map2 proc (Seq.take lenShort wss) <|
                (Seq.take lenShort zss) in
        Seq.fold (fun acc fn ->
            acc && ans.SequenceEqual (fn proc wss zss)) true
            [Seqops.map2R; Seqops.map2I]
        && Seq.fold (fun acc fn ->
            acc && ans.SequenceEqual (fn proc wss zss)) true
            [Listops.map2R; Listops.map2I; ListopsHi.map2F; ListopsHi.map2U
            ; ListopsHi.map2Lc]
    
    [<Property>]
    let ``zipProp`` (xs:int list) (ys:int list) =
        let (wss, zss) = (List.truncate 20 xs, List.truncate 20 ys) in
        let lenShort =
            match Seq.length wss < Seq.length zss with
            | true -> Seq.length wss | _ -> Seq.length zss in
        let ans = Seq.zip (Seq.take lenShort wss) (Seq.take lenShort zss) in
        Seq.fold (fun acc fn ->
            acc && ans.SequenceEqual (fn wss zss)) true
            [Seqops.zipR; Seqops.zipI]
        && Seq.fold (fun acc fn ->
            acc && ans.SequenceEqual (fn wss zss)) true
            [Listops.zipR; Listops.zipI; ListopsHi.zipF; ListopsHi.zipU
            ; ListopsHi.zipLc]
    
    [<Property>]
    let ``concatProp`` (xs:int list list) (ys:int list list list) =
        let (nlst2, nlst3) = (List.truncate 20 xs, List.truncate 20 ys) in
        let (ans2, ans3) = (List.concat nlst2, List.concat nlst3) in
        Seq.fold (fun acc fn -> acc && ans2.SequenceEqual (fn nlst2)) true
            [Seqops.concatR; Seqops.concatI]
        && Seq.fold (fun acc fn -> acc && ans3.SequenceEqual (fn nlst3)) true
            [Seqops.concatR; Seqops.concatI]
        && Seq.fold (fun acc fn -> acc && ans2 = (fn nlst2)) true
            [Listops.concatR; Listops.concatI; ListopsHi.concatF
            ; ListopsHi.concatU]
        && Seq.fold (fun acc fn -> acc && ans3 = (fn nlst3)) true
            [Listops.concatR; Listops.concatI; ListopsHi.concatF
            ; ListopsHi.concatU]
    
    
    [<Property>]
    let ``unzipProp`` (xs:(int * int) list) =
        let lst = List.truncate 20 xs in
        let ans = List.unzip lst in
        Seq.fold (fun acc fn -> acc &&
            (fun (ah:int list, at:int list) (bh, bt) ->
                ah.SequenceEqual bh && at.SequenceEqual bt) ans (fn lst)) true
            [Listops.unzip2I; ListopsHi.unzip2F; ListopsHi.unzip2U]

namespace Introfs.Practice.Tests

open System
open NUnit.Framework
open FsUnit

open Introfs.Practice

[<TestFixture>]
module TcSequenceops =

    module Util = Introfs.Util.Library
    module Seqops = Sequenceops

    let (modNm, epsilon) = ("TcSequenceops", 0.001)
    let curry f a b = f(a, b)
    let curry3 f a b c = f(a, b, c)
    let (lst, revlst) = ([0 .. 4], [4 .. -1 .. 0])

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
    let ``tabulateTest`` () =
        Seq.iter (fun cnt ->
            let (ans1, ans2) = (List.init cnt float,
                List.init cnt (fun e -> 32.0 / 2.0 ** (float e))) in
            Seq.iter (fun fn ->
                ans1 |> should equal <| fn float cnt
                ; ans2 |> should equal <| fn (fun e -> 32.0 / 2.0 ** (float e)) cnt)
                [Seqops.tabulateR; Seqops.tabulateI]

            ; Seq.iter (fun fn ->
                ans1 |> should equal <| fn float cnt
                ; ans2 |> should equal <| fn (fun e -> 32.0 / 2.0 ** (float e)) cnt)
                [Listops.tabulateR; Listops.tabulateI]
            ) [3; 5; 7]

    [<Test>]
    let ``lengthTest`` () =
        Seq.iter (fun n ->
            let (xss, yss) = ([0 .. (n - 1)], [(n - 1) .. -1 .. 0]) in
            Seq.iter (fun fn ->
                Seq.length xss |> should equal <| fn xss
                ; Seq.length yss |> should equal <| fn yss)
                [Seqops.lengthR; Seqops.lengthI; Listops.lengthR
                ; Listops.lengthI]
            ) [3; 5; 7]

    [<Test>]
    let ``nthTest`` () =
        Seq.iter (fun xs ->
            Seq.iter (fun fn ->
                Seq.item 3 xs |> should equal <| (Option.defaultValue -1 <| fn 3 xs))
                [Seqops.nthR; Seqops.nthI; Listops.nthR; Listops.nthI]
            ) [lst; revlst]

    [<Test>]
    let ``indexFindTest`` () =
        let el = 3 in
        let pred item = item = el in
        Seq.iter (fun (xs:int list) ->
            let cslst = ResizeArray<_>(xs) in
            Seq.findIndex pred cslst |> should equal <|
                curry SequenceopsCs.IndexOfLp el cslst
            ; Seq.iter (fun (fnI, fnF) ->
                Seq.tryFindIndex pred cslst |> should equal <| fnI pred cslst
                ; Seq.tryFind pred cslst |> should equal <| fnF pred cslst)
                [(Seqops.findIndexR, Seqops.findR)
                ; (Seqops.findIndexI, Seqops.findI)]

            ; Seq.iter (fun (fnI, fnF) ->
                Seq.tryFindIndex pred xs |> should equal <| fnI pred xs
                ; Seq.tryFind pred xs |> should equal <| fnF pred xs)
                [(Seqops.findIndexR, Seqops.findR)
                ; (Seqops.findIndexI, Seqops.findI)
                ; (Listops.findIndexR, Listops.findR)
                ; (Listops.findIndexI, Listops.findI)]
            ) [lst; revlst]

    [<Test>]
    let ``minMaxTest`` () =
        Seq.iter (fun xs ->
            Seq.iter (fun (fnMin, fnMax) ->
                List.min xs |> should equal <| fnMin xs
                ; List.max xs |> should equal <| fnMax xs)
                [(Seqops.minR, Seqops.maxR); (Seqops.minI, Seqops.maxI)
                ; (Listops.minR, Listops.maxR); (Listops.minI, Listops.maxI)]
            ) [lst; revlst]

    [<Test>]
    let ``reverseTest`` () =
        let lst = [0; 1; 2; 3; 4] in
        let cslst = ResizeArray<_>(lst) in
        (*wrapTest (fun () -> markFunc "SetUp" "reverseTest")
            (fun () -> markFunc "TearDown" "reverseTest")
            (fun () ->
                Seq.iter (fun fn -> Seq.rev lst |> should equal <| fn lst)
                    [Seqops.revR; Seqops.revI])*)
        Seq.iter (fun fn ->
            let tmp = SequenceopsCs.CopyOf(cslst) in
            fn(tmp) ; Seq.rev cslst |> should equal tmp)
            [SequenceopsCs.ReverseLp]
        ; Seq.iter (fun fn -> Seq.rev lst |> should equal <| fn lst)
            [Seqops.revR; Seqops.revI]
        ; Seq.iter (fun fn -> Seq.rev lst |> should equal <| fn lst)
            [Listops.revR; Listops.revI]

    [<Test>]
    let ``copyTest`` () =
        Seq.iter (fun (xs:int list) ->
            Seq.iter (fun fn ->
                Seq.map id xs |> should equal <| fn xs)
                [Seqops.copyR; Seqops.copyI]

            ; Seq.iter (fun fn ->
                List.map id xs |> should equal <| fn xs)
                [Listops.copyR; Listops.copyI]
            ) [lst; revlst]

    [<Test>]
    let ``takeDropTest`` () =
        let n = 3 in
        Seq.iter (fun (xs:int list) ->
            Seq.iter (fun (fnT, fnD) ->
                Seq.take n xs |> should equal <| fnT n xs
                ; Seq.skip n xs |> should equal <| fnD n xs)
                [(Seqops.takeI, Seqops.dropI)]

            ; Seq.iter (fun (fnT, fnD) ->
                List.take n xs |> should equal <| fnT n xs
                ; List.skip n xs |> should equal <| fnD n xs)
                [(Listops.takeI, Listops.dropI)]
            ) [lst; revlst]

    [<Test>]
    let ``existsForallTest`` () =
        let (pred1, pred2) = ((fun el -> 0 = (el % 2)),
            (fun el -> not <| Seq.isEmpty el)) in
        let (lst1, lst2) = ([1; 2; 3],
            [[1; 2]; []; [3; 4]]) in
        let (lst3, lst4) = ([6; 2; 4],
            [[1; 2]; [5]; [3; 4]]) in
        Seq.iter (fun (fnE, fnA) ->
            List.exists pred1 lst1 |> should equal <| fnE pred1 lst1
            ; List.forall pred1 lst3 |> should equal <| fnA pred1 lst3)
           [(Seqops.existsR, Seqops.forallR)
           ; (Seqops.existsI, Seqops.forallI)
           ; (Listops.existsR, Listops.forallR)
           ; (Listops.existsI, Listops.forallI)]
        ; Seq.iter (fun (fnE, fnA) ->
            List.exists pred2 lst2 |> should equal <| fnE pred2 lst2
            ; List.forall pred2 lst4 |> should equal <| fnA pred2 lst4)
           [(Seqops.existsR, Seqops.forallR)
           ; (Seqops.existsI, Seqops.forallI)
           ; (Listops.existsR, Listops.forallR)
           ; (Listops.existsI, Listops.forallI)]

    [<Test>]
    let ``mapTest`` () =
        let proc = (fun el -> el + 2) in
        Seq.iter (fun (xs:int list) ->
            Seq.iter (fun fn ->
                Seq.map proc xs |> should equal <| fn proc xs)
                [Seqops.mapR; Seqops.mapI]

            ; Seq.iter (fun fn ->
                List.map proc xs |> should equal <| fn proc xs)
                [Listops.mapR; Listops.mapI]
            ) [lst; revlst]

    [<Test>]
    let ``iterTest`` () =
        let proc = (fun el -> Printf.printf "%d " el) in
        Seq.iter (fun (xs:int list) ->
            Seq.iter (fun fn ->
                List.iter proc xs |> should equal <| fn proc xs)
                [Seqops.iterR; Seqops.iterI; Listops.iterR
                ; Listops.iterI]
            ) [lst; revlst]

    [<Test>]
    let ``filterRemoveTest`` () =
        let pred1 = (fun el -> 0 = (el % 2)) in
        Seq.iter (fun (xs:int list) ->
            Seq.iter (fun (fnF, fnR) ->
                Seq.filter pred1 xs |> should equal <| fnF pred1 xs
                ; Seq.filter (pred1 >> not) xs |> should equal <| fnR pred1 xs)
               [(Seqops.filterR, Seqops.removeR)
                ; (Seqops.filterI, Seqops.removeI)]

            ; Seq.iter (fun (fnF, fnR) ->
                List.filter pred1 xs |> should equal <| fnF pred1 xs
                ; List.filter (pred1 >> not) xs |> should equal <| fnR pred1 xs)
               [(Listops.filterR, Listops.removeR)
                ; (Listops.filterI, Listops.removeI)]
            ) [lst; revlst]

    [<Test>]
    let ``foldlFoldrTest`` () =
        let (corp1, corp2) = ((fun a e -> a + e), (fun a e -> a - e)) in
        let (proc1, proc2) = ((fun e a -> e + a), (fun e a -> e - a)) in
        Seq.iter (fun xs ->
            Seq.iter (fun (fnL, fnR) ->
                Seq.fold corp1 0 xs |> should equal <| fnL corp1 0 xs
                ; Seq.fold corp2 0 xs |> should equal <| fnL corp2 0 xs

                ; Seq.foldBack proc1 xs 0 |> should equal <| fnR proc1 xs 0
                ; Seq.foldBack proc2 xs 0 |> should equal <| fnR proc2 xs 0)
                [(Seqops.foldLeftR, Seqops.foldRightR)
                ; (Seqops.foldLeftI, Seqops.foldRightI)
                ; (Listops.foldLeftR, Listops.foldRightR)
                ; (Listops.foldLeftI, Listops.foldRightI)]
            ) [lst; revlst]

    [<Test>]
    let ``unfoldrUnfoldlTest`` () =
        let funcRange (start, stop) =
            match stop < start with
            | true -> None | _ -> Some (start, (start + 1, stop)) in
        let funcUnsum (start, stop) =
            match stop < start with
            | true -> None | _ -> Some (start, (start + 1, stop - start)) in
        let funcBsexpand (b, num) =
            match num <= 0 with
            | true -> None | _ -> Some (num % b, (b, num / b)) in
        let funcFib (s0, s1, num) =
            match num < 0 with
            | true -> None | _ -> Some (s0, (s0 + s1, s0, num - 1)) in
        revlst |> should equal <| Seqops.unfoldRightI funcRange (0, 4)
        ; lst |> should equal <| Seqops.unfoldLeftR funcRange (0, 4)
        ; Seq.rev (Seq.unfold funcUnsum (0, 10)) |> should equal <|
            Seqops.unfoldRightI funcUnsum (0, 10)
        ; Seq.unfold funcUnsum (0, 10) |> should equal <|
            Seqops.unfoldLeftR funcUnsum (0, 10)
        ; Seq.rev (Seq.unfold funcBsexpand (2, 64)) |> should equal <|
            Seqops.unfoldRightI funcBsexpand (2, 64)
        ; Seq.unfold funcBsexpand (2, 64) |> should equal <|
            Seqops.unfoldLeftR funcBsexpand (2, 64)
        ; Seq.rev (Seq.unfold funcFib (0, 1, 13)) |> should equal <|
            Seqops.unfoldRightI funcFib (0, 1, 13)
        ; Seq.unfold funcFib (0, 1, 13) |> should equal <|
            Seqops.unfoldLeftR funcFib (0, 1, 13)

        revlst |> should equal <| Listops.unfoldRightI funcRange (0, 4)
        ; lst |> should equal <| Listops.unfoldLeftR funcRange (0, 4)
        ; Seq.rev (Seq.unfold funcUnsum (0, 10)) |> should equal <|
            Listops.unfoldRightI funcUnsum (0, 10)
        ; Seq.unfold funcUnsum (0, 10) |> should equal <|
            Listops.unfoldLeftR funcUnsum (0, 10)
        ; Seq.rev (Seq.unfold funcBsexpand (2, 64)) |> should equal <|
            Listops.unfoldRightI funcBsexpand (2, 64)
        ; Seq.unfold funcBsexpand (2, 64) |> should equal <|
            Listops.unfoldLeftR funcBsexpand (2, 64)
        ; Seq.rev (Seq.unfold funcFib (0, 1, 13)) |> should equal <|
            Listops.unfoldRightI funcFib (0, 1, 13)
        ; Seq.unfold funcFib (0, 1, 13) |> should equal <|
            Listops.unfoldLeftR funcFib (0, 1, 13)

    [<Test>]
    let ``isOrderedTest`` () =
        let verifyOrder cmpfn keyfn lst =
            match Seq.isEmpty lst with
            | true -> true
            | _ ->
                let (x, xs) = (Seq.head lst, Seq.tail lst) in
                fst <| Seq.fold (fun (a, cur) e ->
                    ((cmpfn cur (keyfn e)) && a, (keyfn e))) (true, x) lst in
        Seq.iter (fun xs ->
            Seq.iter (fun fn ->
                verifyOrder (<=) id xs |> should equal <| fn (<=) id xs
                ; verifyOrder (>=) id xs |> should equal <| fn (>=) id xs)
                [Seqops.isOrderedR; Seqops.isOrderedI
                ; Listops.isOrderedR; Listops.isOrderedI]
           ) [lst; revlst]
        ; Seq.iter (fun xs ->
            Seq.iter (fun fn ->
                verifyOrder (<=) id xs |> should equal <| fn (<=) id xs
                ; verifyOrder (>=) id xs |> should equal <| fn (>=) id xs)
                [Seqops.isOrderedR; Seqops.isOrderedI
                ; Listops.isOrderedR; Listops.isOrderedI]
           ) [['a'; 'c'; 'e']; ['9'; '5'; '2']]


    [<Test>]
    let ``appendTest`` () =
        let nines = [9; 9; 9; 9] in
        Seq.iter (fun (xs:int list) ->
            Seq.iter (fun fn ->
                List.append xs nines |> should equal <| fn xs nines)
                [Seqops.appendR; Seqops.appendI]

            ; Seq.iter (fun fn ->
                Seq.append xs nines |> should equal <| fn xs nines)
                [Listops.appendR; Listops.appendI]
            ) [lst; revlst]

    [<Test>]
    let ``interleaveTest`` () =
        let nines = [9; 9; 9; 9] in
        Seq.iter (fun fn ->
            [0; 9; 1; 9; 2; 9; 3; 9; 4] |> should equal <| fn lst nines
            ; [4; 9; 3; 9; 2; 9; 1; 9; 0] |> should equal <| fn revlst nines)
            [Seqops.interleaveR; Seqops.interleaveI]

        ; Seq.iter (fun fn ->
            [0; 9; 1; 9; 2; 9; 3; 9; 4] |> should equal <| fn lst nines
            ; [4; 9; 3; 9; 2; 9; 1; 9; 0] |> should equal <| fn revlst nines)
            [Listops.interleaveR; Listops.interleaveI]

    [<Test>]
    let ``map2Test`` () =
        let proc = (fun e1 e2 -> (e1 + e2) + 2) in
        Seq.iter (fun (xs:int list) ->
            Seq.iter (fun fn ->
                Seq.map2 proc xs xs |> should equal <| fn proc xs xs)
                [Seqops.map2R; Seqops.map2I]

            ; Seq.iter (fun fn ->
                List.map2 proc xs xs |> should equal <| fn proc xs xs)
                [Listops.map2R; Listops.map2I]
            ) [lst; revlst]

    [<Test>]
    let ``zipTest`` () =
        let (lst1, lst2) = ([0; 1; 2], [20; 30; 40]) in
        Seq.iter (fun fn ->
            Seq.zip lst1 lst2 |> should equal <| fn lst1 lst2)
            [Seqops.zipR; Seqops.zipI]

        ; Seq.iter (fun fn ->
            List.zip lst1 lst2 |> should equal <| fn lst1 lst2)
            [Listops.zipR; Listops.zipI]

    [<Test>]
    let ``concatTest`` () =
        let (nlst1, nlst2) = ([[0; 1; 2]; [20; 30]],
            [[[0; 1]]; []; [[20; 30]]]) in
        Seq.iter (fun fn ->
            Seq.concat nlst1 |> should equal <| fn nlst1)
            [Seqops.concatR; Seqops.concatI]
        ; Seq.iter (fun fn ->
            Seq.concat nlst2 |> should equal <| fn nlst2)
            [Seqops.concatR; Seqops.concatI]

        ; Seq.iter (fun fn ->
            List.concat nlst1 |> should equal <| fn nlst1)
            [Listops.concatR; Listops.concatI]
        ; Seq.iter (fun fn ->
            List.concat nlst2 |> should equal <| fn nlst2)
            [Listops.concatR; Listops.concatI]


    [<Test>]
    let ``unzipTest`` () =
        let lst1 = [(0, 20); (1, 30)] in
        Seq.iter (fun fn ->
            List.unzip lst1 |> should equal <| fn lst1)
            [Listops.unzip2I]

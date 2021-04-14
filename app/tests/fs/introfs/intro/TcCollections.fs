namespace Introfs.Intro.Tests

open System
open System.Linq
open NUnit.Framework
open FsUnit

[<TestFixture>]
module TcCollections =

    module Util = Introfs.Util.Library

    let (modNm, epsilon) = ("TcCollections", 0.001)

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

    [<Test>] [<Category("Tag4")>]
    let ``ListTest`` () =
        let lst = [2; 1; 0; 4; 3] in
        1 :: [] |> should equal [1]
        lst |> should equal [2; 1; 0; 4; 3]
        lst |> should not' (equal [2; 1; 0])
        [] |> should be Empty
        List.head lst |> should equal 2
        List.item 2 lst |> should equal 0
        List.length lst |> should equal 5
        lst @ [9; 9; 9; 9] |> should equal [2; 1; 0; 4; 3; 9; 9; 9; 9]
        List.rev lst |> should equal [3; 4; 0; 1; 2]
        List.contains 1 lst |> should equal true
        List.exists (fun x -> 1 = x % 2) lst |> should equal true
        List.forall (fun x -> 0 = x % 2) [6; 4; 2] |> should equal true
        List.filter (fun x -> 0 = x % 2) lst |> should equal [2; 0; 4]
        List.partition (fun x -> 0 = x % 2) lst |> snd |> should equal [1; 3]
        List.fold (fun a e -> a - e) 0 lst |> should equal -10
        List.foldBack (fun e a -> e - a) lst 0 |> should equal 0
        List.map (fun e -> e + 2) lst |> should equal [4; 3; 2; 6; 5]
        Array.ofList lst |> should equal [|2; 1; 0; 4; 3|]
        List.sortWith (fun a b -> compare b a) lst |> should equal [4; 3; 2; 1; 0]

    [<Test>] [<Category("Tag4")>]
    let ``SeqTest`` () =
        let xs = seq [2; 1; 0; 4; 3] in
        Seq.append Seq.empty (seq [1]) |> should equal <| seq [1]
        xs |> should equal <| seq [2; 1; 0; 4; 3]
        xs |> should not' (equal (seq [2; 1; 0]))
        Seq.empty |> should be Empty
        Seq.head xs |> should equal 2
        Seq.item 2 xs |> should equal 0
        Seq.length xs |> should equal 5
        Seq.append xs [9; 9; 9; 9] |> should equal <| seq [2; 1; 0; 4; 3; 9; 9; 9; 9]
        Seq.rev xs |> should equal <| seq [3; 4; 0; 1; 2]
        Seq.contains 1 xs |> should equal true
        Seq.exists (fun x -> 1 = x % 2) xs |> should equal true
        Seq.forall (fun x -> 0 = x % 2) (seq [6; 4; 2]) |> should equal true
        Seq.filter (fun x -> 0 = x % 2) xs |> should equal <| seq [2; 0; 4]
        Seq.filter (fun x -> not (0 = x % 2)) xs |> should equal <| seq [1; 3]
        Seq.fold (fun a e -> a - e) 0 xs |> should equal -10
        Seq.foldBack (fun e a -> e - a) xs 0 |> should equal 0
        Seq.map (fun e -> e + 2) xs |> should equal <| seq [4; 3; 2; 6; 5]
        Seq.toArray xs |> should equal [|2; 1; 0; 4; 3|]
        Seq.sortWith (fun a b -> compare b a) xs |> should equal <| seq [4; 3; 2; 1; 0]

    [<Test>] [<Category("Tag4")>]
    let ``ResizeArrayTest`` () =
        let oolst = ResizeArray<_> [2; 1; 0; 4; 3] in
        oolst |> should equal <| ResizeArray<_> [2; 1; 0; 4; 3]
        oolst |> should not' (equal (ResizeArray<_> [2; 1; 0]))
        ResizeArray<_>() |> should be Empty
        oolst.Item 0 |> should equal 2
        oolst.Item 2 |> should equal 0
        oolst.Count |> should equal 5
        oolst.Reverse (); oolst |> should equal <| ResizeArray<_> [3; 4; 0; 1; 2]
        oolst.Contains 1 |> should equal true
        oolst.Exists (fun x -> 1 = x % 2) |> should equal true
        (ResizeArray<_> [6; 4; 2]).TrueForAll (fun x -> 0 = x % 2) |> should equal true
        oolst.FindAll (fun x -> 0 = x % 2) |> should equal <| ResizeArray<_> [4; 0; 2]
        oolst.FindAll (fun x -> not (0 = x % 2)) |> should equal <| ResizeArray<_> [3; 1]
        oolst.Aggregate (0, (fun a e -> a - e)) |> should equal -10
        oolst.Select (fun e -> e + 2) |> should equal <| ResizeArray<_> [5; 6; 2; 3; 4]
        oolst.ToArray () |> should equal [|3; 4; 0; 1; 2|]
        oolst.Sort (fun a b -> compare b a) ; oolst |> should equal <| ResizeArray<_> [4; 3; 2; 1; 0]
        oolst.AddRange [9; 9; 9; 9]
        oolst |> should equal <| ResizeArray<_> [4; 3; 2; 1; 0; 9; 9; 9; 9]

    [<Test>] [<Category("Tag4")>]
    let ``SetTest`` () =
        let (set1, set2) = (ref Set.empty, ref <| set ['q'; 'p'; 'z'; 'u']) in
        let lst1 = ['a'; 'e'; 'k'; 'p'; 'u'; 'k'; 'a'] in
        let xor_set setA setB =
            Set.difference (Set.union setA setB) (Set.intersect setA setB) in
        List.iter (fun e -> set1 := Set.add e !set1 ; ()) lst1
        !set1 |> should equal <| set ['a'; 'e'; 'k'; 'p'; 'u']
        !set1 |> should not' (equal (set ['a'; 'e'; 'k']))
        Set.empty |> should be Empty
        Set.count !set1 |> should equal 5
        Set.add 'd' !set1 |> should equal <| set ['a'; 'e'; 'k'; 'p'; 'u'; 'd']
        Set.union !set1 !set2 |> should equal <| set ['a'; 'e'; 'k'; 'p'; 'u'; 'q'; 'z']
        Set.intersect !set1 !set2 |> should equal <| set ['p'; 'u']
        Set.difference !set1 !set2 |> should equal <| set ['a'; 'e'; 'k']
        xor_set !set1 !set2 |> should equal <| set ['a'; 'e'; 'k'; 'q'; 'z']
        Set.toList !set1 |> should equal ['a'; 'e'; 'k'; 'p'; 'u']

    [<Test>] [<Category("Tag4")>]
    let ``MapTest`` () =
        let map1 = ref Map.empty in
        let lst_str = ["ltr 0"; "ltr 1"; "ltr 2"; "ltr 3"; "ltr 4"; "ltr 5"; "ltr 6"] in
        let lst_char = ['a'; 'e'; 'k'; 'p'; 'u'; 'k'; 'a'] in
        !map1 |> should be Empty
        List.iter (fun (k, v) -> map1 := Map.add k v !map1) <|
            List.zip lst_str lst_char
        !map1 |> should equal <| Map [("ltr 0", 'a'); ("ltr 1", 'e')
            ; ("ltr 2", 'k'); ("ltr 3", 'p'); ("ltr 4", 'u')
            ; ("ltr 5", 'k'); ("ltr 6", 'a')]
        Map.containsKey "ltr 1" !map1 |> should equal true
        Map.containsKey "ltr 1" (Map.remove "ltr 1" !map1) |> should equal false
        Map.count !map1 |> should equal 7
        Map.toList !map1 |> should equal [("ltr 0", 'a'); ("ltr 1", 'e')
            ; ("ltr 2", 'k'); ("ltr 3", 'p'); ("ltr 4", 'u')
            ; ("ltr 5", 'k'); ("ltr 6", 'a')]

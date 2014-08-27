namespace Introfs.Intro.Tests
    
open System
open System.Linq
open NUnit.Framework
open FsCheck

[<TestFixture>]
module TpCollections =
    
    module Util = Introfs.Util.Library
    type PropertyAttribute = FsCheck.NUnit.PropertyAttribute
    //type MyArbitraries = Base.MyArbitraries
    
    let (modNm, epsilon) = ("TpCollections", 0.001)
    
    [<Property>] [<Category("Tag3")>]
    let ``consProp`` (x:int) (xs: int list) =
        x = List.head (x :: xs)
    
    [<Property>] [<Category("Tag3")>]
    let ``nullProp`` (xs: int list) =
        (0 = List.length xs) = ([] = xs)
    
    [<Property>] [<Category("Tag3")>]
    let ``equalProp`` (xs: int list) =
        let ys = List.map (fun e -> e) xs in
        fst <| List.fold (fun (a, mss) e -> 
            match mss with
            | [] -> (a, [])
            | m::ms -> (a && m = e, ms)) (true, ys) xs
    
    [<Property>] [<Category("Tag3")>]
    let ``appendProp`` (xs: int list) (ys: int list) =
        (xs @ ys) = List.foldBack (fun e acc -> e::acc) xs ys
    
    [<Property>] [<Category("Tag3")>]
    let ``revProp`` (xs: int list) =
        xs = (List.rev <| List.rev xs)
    
    [<Property>] [<Category("Tag3")>]
    let ``filterProp`` (xs: int list) =
        let pred1 = (fun e -> 0 = e % 2) in
        List.forall pred1 <| List.filter pred1 xs
    
    [<Property>] [<Category("Tag3")>]
    let ``removeProp`` (xs: int list) =
        let pred1 = (fun e -> 0 = e % 2) in
        List.forall pred1 <| fst (List.partition pred1 xs)
    
    [<Property>] [<Category("Tag3")>]
    let ``mapProp`` (xs: int list) =
        let proc1 = (fun e -> e + 2) in
        let ys = List.map proc1 xs in
        fst <| List.fold (fun (a, mss) e -> 
            match mss with
            | [] -> (a, [])
            | m::ms -> (a && m = (proc1 e), ms)) (true, ys) xs
    
    let rec isOrdered (cmpfn: 'a -> 'a -> bool) (xs: 'a list) =
        match xs with
        | [] -> true
        | _::[] -> true
        | y::z::zs -> (cmpfn y z) && (isOrdered cmpfn (z::zs))
    
    [<Property>] [<Category("Tag3")>]
    let ``sortIsOrderedProp`` (xs: int list) =
        isOrdered (<=) <| List.sortWith compare xs
    
    [<Property>] [<Category("Tag3")>]
    let ``revSortIsRevOrderedProp`` (xs: int list) =
        isOrdered (>=) <| List.sortWith (fun a b -> compare b a) xs

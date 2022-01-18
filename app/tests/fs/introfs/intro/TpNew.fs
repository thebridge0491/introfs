namespace Introfs.Intro.Tests
    
open System
open System.Linq
open NUnit.Framework
open FsCheck

open Introfs.Intro

[<TestFixture>]
module TpNew =
    
    type PropertyAttribute = FsCheck.NUnit.PropertyAttribute
    type MyArbitraries = Base.MyArbitraries
    
    let (modNm, epsilon) = ("TpNew", 0.001)
    
    // nunit spec style - call quickcheck
    [<Test>] [<Category("Tag3")>]
    let ``commutAddCheck`` () = 
        let func (a: int) (b: int) = (a + b) = (b + a) in
        (*Fluent.Spec.ForAny<int, int>(func). QuickCheckThrowOnFailure()*)
        Check.One (
            {Config.VerboseThrowOnFailure with
                MaxTest = 20}, func)
    
    [<Test>] [<Category("Tag3")>]
    let ``assocAddCheck`` () = 
        let func (x: int) (y: int) (z: int) = 
            let (a, b, c) = (float x, float y, float z) in
            Library.inEpsilon (epsilon * ((a + b) + c)) ((a + b) + c) (a + (b + c)) in
        (*Fluent.Spec.ForAny<int, int, int>(func
            ).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func
    
    [<Test>] [<Category("Tag3")>]
    let ``revRevCheck`` () = 
        let func (xs: int[]) = xs = Array.rev (Array.rev xs) in
        (*Fluent.Spec.ForAny<int[]>(func).QuickCheckThrowOnFailure() ;*)
        Check.QuickThrowOnFailure func ;
        (*Fluent.Spec.For(Gen.arrayOf (Gen.choose(-10, 10)),
            func).When(fun xs -> xs.Length > 0).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure (fun () ->
            let xs = List.item 0 (Gen.sample 20 1 (Gen.arrayOf (Gen.choose(-10, 10)) |> Gen.suchThat (fun xs -> xs.Length > 0))) in
            func xs)
    
    [<Test>]
    let ``idRevCheck`` () = 
        let func (xs: float[]) = xs.SequenceEqual (Array.rev xs) in
        (*Fluent.Spec.ForAny<float[]>(func).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func
    
    [<Test>] [<Category("Tag3")>]
    let ``sortRevCheck`` () = 
        let func (xs: float[]) = 
            let ys = Array.sort <| Array.copy xs in
            let zs = Array.sort <| Array.rev xs in
            ys.SequenceEqual zs in
        (*Fluent.Spec.ForAny<float[]>(func).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func
    
    [<Test>] [<Category("Tag3")>]
    let ``minSortHeadCheckA`` () = 
        let func (xs: NonEmptyArray<NormalFloat>) = 
            let ys = (xs.Get) |> Array.map (fun e -> e.Get) |> Array.sort in
            Library.inEpsilon (epsilon * ys.[0]) ys.[0] (Array.min xs.Get).Get in
        (*Fluent.Spec.ForAny<NonEmptyArray<NormalFloat>>(func).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func
    
    [<Test>] [<Category("Tag3")>]
    let ``minSortHeadCheckB`` () = 
        let func (xs: seq<float>) = 
            let item0 = Seq.item 0 <| Seq.sort xs in
            Library.inEpsilon (epsilon * item0) item0 (Seq.min xs) in
        (*Fluent.DefaultArbitraries.Add<MyArbitraries> () |> ignore ;
        Fluent.Spec.ForAny<seq<float>>(func).QuickCheckThrowOnFailure()*)
        Arb.register<MyArbitraries> () |> ignore ;
        Check.QuickThrowOnFailure func
    
    // fscheck property style
    [<Property(MaxTest = 20, Verbose = true)>] [<Category("Tag3")>]
    let ``commutAddProp`` (a: int) (b: int) =
        (a + b) = (b + a)
    
    [<Property>] [<Category("Tag3")>]
    let ``assocAddProp`` (x: int) (y: int) (z: int) = 
        let (a, b, c) = (float x, float y, float z) in
        Library.inEpsilon (epsilon * ((a + b) + c)) ((a + b) + c) (a + (b + c))
    
    [<Property>] [<Category("Tag3")>]
    let ``revRevProp`` (xs: int[]) = 
        let ys = List.item 0 (Gen.sample 20 1 (Gen.arrayOf (Gen.choose(-10, 10)) |> Gen.suchThat (fun ys -> ys.Length > 0))) in
        xs = Array.rev (Array.rev xs) && ys = Array.rev (Array.rev ys)
    
    [<Property>]
    let ``idRevProp`` (xs: float[]) = 
        xs.SequenceEqual (Array.rev xs)
    
    [<Property>] [<Category("Tag3")>]
    let ``sortRevProp`` (xs: float[]) = 
        let ys = Array.sort <| Array.copy xs in
        let zs = Array.sort <| Array.rev xs in
        ys.SequenceEqual zs
    
    [<Property>] [<Category("Tag3")>]
    let ``minSortHeadPropA`` (xs: NonEmptyArray<NormalFloat>) = 
        let ys = xs.Get |> Array.map (fun e -> e.Get) |> Array.sort in
        Library.inEpsilon (epsilon * ys.[0]) ys.[0] (Array.min xs.Get).Get
    
    [<Property(Arbitrary = [| typeof<MyArbitraries> |])>] [<Category("Tag3")>]
    let ``minSortHeadPropB`` (xs: seq<float>) = 
        let item0 = Seq.item 0 <| Seq.sort xs in
        Library.inEpsilon (epsilon * item0) item0 (Seq.min xs)
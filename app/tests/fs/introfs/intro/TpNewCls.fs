namespace Introfs.Intro.Tests
    
open System
open System.Linq
open NUnit.Framework
open FsCheck

open Introfs.Intro

type PropertyAttribute = FsCheck.NUnit.PropertyAttribute
type MyArbitraries = Base.MyArbitraries

[<TestFixture>]
type TpNewCls () =
    inherit Base.ClsBase ()
    
    let (epsilon) = (0.001)  //(1.20e-7)
    
    (*member this.d = Base.Misc.getdisposable (fun () ->
        printf "Derived Dispose(%A)\n" <| this.GetType().BaseType)
    interface System.IDisposable with
        member this.Dispose () = this.d.Dispose () ; base.d.Dispose ()*)
    
    // nunit spec style - call quickcheck
    [<Test>] [<Category("Tag3")>]
    member this.CommutAddCheck () = 
        let func (a: int) (b: int) = (a + b) = (b + a) in
        (*Fluent.Spec.ForAny<int, int>(func). QuickCheckThrowOnFailure()*)
        Check.One (
            {Config.VerboseThrowOnFailure with
                MaxTest = 20}, func)
    
    [<Test>] [<Category("Tag3")>]
    member this.AssocAddCheck () = 
        let func (x: int) (y: int) (z: int) = 
            let (a, b, c) = (float x, float y, float z) in
            Library.inEpsilon (epsilon * ((a + b) + c)) ((a + b) + c) (a + (b + c)) in
        (*Fluent.Spec.ForAny<int, int, int>(func
            ).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func
    
    [<Test>] [<Category("Tag3")>]
    member this.RevRevCheck () = 
        let func (xs: int[]) = xs = Array.rev (Array.rev xs) in
        (*Fluent.Spec.ForAny<int[]>(func).QuickCheckThrowOnFailure() ;*)
        Check.QuickThrowOnFailure func ;
        (*Fluent.Spec.For(Gen.arrayOf (Gen.choose(-10, 10)),
            func).When(fun xs -> xs.Length > 0).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure (fun () ->
            let xs = List.item 0 (Gen.sample 20 1 (Gen.arrayOf (Gen.choose(-10, 10)) |> Gen.suchThat (fun xs -> xs.Length > 0))) in
            func xs)
    
    [<Test>]
    member this.IdRevCheck () = 
        let func (xs: float[]) = xs.SequenceEqual (Array.rev xs) in
        (*Fluent.Spec.ForAny<float[]>(func).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func
    
    [<Test>] [<Category("Tag3")>]
    member this.SortRevCheck () = 
        let func (xs: float[]) = 
            let ys = Array.sort <| Array.copy xs in
            let zs = Array.sort <| Array.rev xs in
            ys.SequenceEqual zs in
        (*Fluent.Spec.ForAny<float[]>(func).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func
    
    [<Test>] [<Category("Tag3")>]
    member this.MinSortHeadCheckA () = 
        let func (xs: NonEmptyArray<NormalFloat>) = 
            let ys = (xs.Get) |> Array.map (fun e -> e.Get) |> Array.sort in
            Library.inEpsilon (epsilon * ys.[0]) ys.[0] (Array.min xs.Get).Get in
        (*Fluent.Spec.ForAny<NonEmptyArray<NormalFloat>>(func).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func
    
    [<Test>] [<Category("Tag3")>]
    member this.MinSortHeadCheckB () = 
        let func (xs: seq<float>) = 
            let item0 = Seq.item 0 <| Seq.sort xs in
            Library.inEpsilon (epsilon * item0) item0 (Seq.min xs) in
        (*Fluent.DefaultArbitraries.Add<MyArbitraries> () |> ignore ;
        Fluent.Spec.ForAny<seq<float>>(func).QuickCheckThrowOnFailure()*)
        Arb.register<MyArbitraries> () |> ignore ;
        Check.QuickThrowOnFailure func
    
    // fscheck property style
    [<Property(MaxTest = 20, Verbose = true)>] [<Category("Tag3")>]
    member this.CommutAddProp (a: int) (b: int) =
        (a + b) = (b + a)
    
    [<Property>] [<Category("Tag3")>]
    member this.AssocAddProp (x: int) (y: int) (z: int) = 
        let (a, b, c) = (float x, float y, float z) in
        Library.inEpsilon (epsilon * ((a + b) + c)) ((a + b) + c) (a + (b + c))
    
    [<Property>] [<Category("Tag3")>]
    member this.RevRevProp (xs: int[]) = 
        let ys = List.item 0 (Gen.sample 20 1 (Gen.arrayOf (Gen.choose(-10, 10)) |> Gen.suchThat (fun ys -> ys.Length > 0))) in
        xs = Array.rev (Array.rev xs) && ys = Array.rev (Array.rev ys)
    
    [<Property>]
    member this.IdRevProp (xs: float[]) = 
        xs.SequenceEqual (Array.rev xs)
    
    [<Property>] [<Category("Tag3")>]
    member this.SortRevProp (xs: float[]) = 
        let ys = Array.sort <| Array.copy xs in
        let zs = Array.sort <| Array.rev xs in
        ys.SequenceEqual zs
    
    [<Property>] [<Category("Tag3")>]
    member this.MinSortHeadPropA (xs: NonEmptyArray<NormalFloat>) = 
        let ys = xs.Get |> Array.map (fun e -> e.Get) |> Array.sort in
        Library.inEpsilon (epsilon * ys.[0]) ys.[0] (Array.min xs.Get).Get
    
    [<Property(Arbitrary = [| typeof<MyArbitraries> |])>] [<Category("Tag3")>]
    member this.MinSortHeadPropB (xs: seq<float>) = 
        let item0 = Seq.item 0 <| Seq.sort xs in
        Library.inEpsilon (epsilon * item0) item0 (Seq.min xs)

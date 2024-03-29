namespace Introfs.Util.Tests

open System
open System.Linq
open NUnit.Framework
open FsCheck

open Introfs.Util

[<TestFixture>]
module TpNew =

    type PropertyAttribute = FsCheck.NUnit.PropertyAttribute
    type MyArbitraries = Base.MyArbitraries

    let (modNm, epsilon) = ("TpNew", 0.001)

    // nunit spec style - call quickcheck
    [<Test>] [<Category("Tag3")>]
    let ``commutAddCheck`` () =
        let func (a: int) (b: int) = (a + b) = (b + a) in
        (*Prop.ForAll<int, int>(func). QuickCheckThrowOnFailure()*)
        Check.One (
            {Config.VerboseThrowOnFailure with
                MaxTest = 20}, func)

    [<Test>] [<Category("Tag3")>]
    let ``assocAddCheck`` () =
        let func (x: int) (y: int) (z: int) =
            let (a, b, c) = (float x, float y, float z) in
            Library.inEpsilon (epsilon * ((a + b) + c)) ((a + b) + c) (a + (b + c)) in
        (*Prop.ForAll<int, int, int>(func
            ).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func

    [<Test>] [<Category("Tag3")>]
    let ``revRevCheck`` () =
        let func (xs: int[]) = xs = Array.rev (Array.rev xs) in
        (*Prop.ForAll<int[]>(func).QuickCheckThrowOnFailure() ;*)
        Check.QuickThrowOnFailure func

    [<Test>]
    let ``idRevCheck`` () =
        let func (xs: float[]) = xs.SequenceEqual (Array.rev xs) in
        (*Prop.ForAll<float[]>(func).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func

    [<Test>] [<Category("Tag3")>]
    let ``sortRevCheck`` () =
        let func (xs: float[]) =
            let ys = Array.sort <| Array.copy xs in
            let zs = Array.sort <| Array.rev xs in
            ys.SequenceEqual zs in
        (*Prop.ForAll<float[]>(func).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func

    [<Test>] [<Category("Tag3")>]
    let ``minSortHeadCheckA`` () =
        let func (xs: NonEmptyArray<NormalFloat>) =
            let ys = (xs.Get) |> Array.map (fun e -> e.Get) |> Array.sort in
            Library.inEpsilon (epsilon * ys.[0]) ys.[0] (Array.min xs.Get).Get in
        (*Prop.ForAll<NonEmptyArray<NormalFloat>>(func).QuickCheckThrowOnFailure()*)
        Check.QuickThrowOnFailure func

    [<Test>] [<Category("Tag3")>]
    let ``minSortHeadCheckB`` () =
        let func (xs: seq<float>) =
            let item0 = Seq.item 0 <| Seq.sort xs in
            Library.inEpsilon (epsilon * item0) item0 (Seq.min xs) in
        Arb.register<MyArbitraries> () |> ignore ;
        (*Prop.ForAll<seq<float>>(func).QuickCheckThrowOnFailure()*)
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
        xs = Array.rev (Array.rev xs)

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

namespace Introfs.Util.Tests

open System
open NUnit.Framework
open FsUnit

open Introfs.Util

[<TestFixture>]
type TcNewCls () =                     //as self =
    inherit Base.ClsBase ()

    let (epsilon) = (0.001)  //(1.20e-7)
    //do ...

    member this.d = Base.Misc.getdisposable (fun () ->
        printf "Derived Dispose(%A)\n" <| this.GetType().BaseType)
    interface System.IDisposable with
        member this.Dispose () = this.d.Dispose () ; base.d.Dispose ()

    [<OneTimeSetUp>]
    override this.SetUpClass () =
        base.SetUpClass () ;
        printf "SetUpClass(%A)\n" <| this.GetType() ; ignore ()

    [<OneTimeTearDown>]
    override this.TearDownClass () =
        ignore () ; printf "TearDownClass(%A)\n" <| this.GetType() ;
        base.TearDownClass ()

    [<SetUp>]
    override this.SetUp () =
        base.SetUp () ;
        printf "SetUp(%A)\n" <| this.GetType() ; ignore ()

    [<TearDown>]
    override this.TearDown () =
        ignore () ; printf "TearDown(%A)\n" <| this.GetType() ;
        base.TearDown ()

    member this.MarkFunc(stage, funcNm) =
        printf "%s(%A.%s)\n" stage (this.GetType()) funcNm

    [<Test>] [<Category("Tag1")>]
    member this.MethodTest () =
        this.WrapTest((fun () -> this.MarkFunc("SetUp", "MethodTest")),
            (fun () -> this.MarkFunc("TearDown", "MethodTest")),
            (fun () ->
                //Assert.AreEqual(2 * 2, 4, "Multiply"))
                //Assert.That(2 * 2, Is.EqualTo(4), "(Constraint) Multiply"))
                4 |> should equal (2 * 2)))

    [<Test>] [<Category("Tag1")>]
    member this.FloatTest () =
        //Assert.AreEqual(4.0, 4.0, "Floats")
        //Assert.That(4.0, Is.EqualTo(4.0).Within(4.0 * epsilon),
        //        "Floats constraint-based")
        //Assert.True(Library.inEpsilon *epsilon * 4.0f) 4.0f 4.0f)
        4.0 |> should (equalWithin <| 4.0 * epsilon) 4.0

    [<Test>] [<Category("Tag1")>]
    member this.StringTest () =
        let (str1, str2) = ("Hello", "hello") in
        //StringAssert.AreEqualIgnoringCase(str1, str2, "Strings")
        0 |> should equal <| String.Compare(str1, str2,
            StringComparison.OrdinalIgnoreCase)

    [<Test>] [<Category("Tag2")>]
    member this.BadTest () =
        //Assert.AreEqual(4, 5, "Equals")
        5 |> should equal 4

    [<Test>] [<Category("Tag2")>]
    member this.FailedTest () =
        //Assert.Fail()
        false |> should equal true

    [<Test>] [<Category("Tag2")>] [<Ignore("ignored test")>]
    member this.IgnoredTest () =
        //raise (Exception()) |> ignore
        (fun () -> raise (Exception())) |> should throw typeof<Exception>

    [<Test>] [<Category("Tag2")>] [<Platform("Win98, WinME")>]
    member this.SkippedWinMETest () = ignore () // ...

    [<Test>] [<Category("Tag1")>] //[<Timeout(100)>]
    member this.PassedTest() = ignore () // Assert.Pass()

    [<Test>] //[<ExpectedException("System.InvalidOperationException")>]
    member this.ExpectAnException () =
        //raise (InvalidOperationException()) |> ignore
        (fun () -> raise (InvalidOperationException()) |> ignore) |>
          should throw typeof<InvalidOperationException>

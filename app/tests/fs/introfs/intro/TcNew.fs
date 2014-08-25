namespace Introfs.Intro.Tests
    
open System
open NUnit.Framework
open FsUnit

[<TestFixture>]
module TcNew =
    
    module Util = Introfs.Util.Library
    
    let (modNm, epsilon) = ("TcNew", 0.001)
    
    [<TestFixtureSetUp>]
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
        ignore () ; printf "tearDown(%s)\n" modNm
    
    
    let wrapTest startFun endFun testFun =
        startFun () ; testFun () ; endFun ()
    
    let markFunc stage funcNm =
        printf "%s(%s.%s)\n" stage modNm funcNm
    
    [<Test>] [<Category("Tag1")>]
    let ``funcTest`` () = 
        wrapTest (fun () -> markFunc "setUp" "funcTest")
            (fun () -> markFunc "tearDown" "funcTest")
            (fun () -> 
                //Assert.AreEqual(2 * 2, 4, "Multiply"))
                4 |> should equal (2 * 2))
    
    [<Test>] [<Category("Tag1")>]
    let ``floatTest`` () = 
        //Assert.AreEqual(4.0, 4.0, "Floats")
        //Assert.That(4.0, Is.EqualTo(4.0).Within(4.0 * epsilon), 
        //        "Floats constraint-based")
        //Assert.True(Util.inEpsilon * epsilon * 4.0f) 4.0f 4.0f)
        4.0 |> should (equalWithin <| 4.0 * epsilon) 4.0
    
    [<Test>] [<Category("Tag1")>]
    let ``stringTest`` () = 
        let (str1, str2) = ("Hello", "hello") in
        //StringAssert.AreEqualIgnoringCase(str1, str2, "Strings")
        0 |> should equal <| String.Compare(str1, str2, 
            StringComparison.OrdinalIgnoreCase)
    
    [<Test>] [<Category("Tag2")>]
    let ``badTest`` () = 
        //Assert.AreEqual(4, 5, "Equals")
        5 |> should equal 4
    
    [<Test>] [<Category("Tag2")>]
    let ``failedTest`` () = 
        //Assert.Fail()
        false |> should equal true
    
    [<Test>] [<Category("Tag2")>] [<Ignore("ignored test")>] 
    let ``ignoredTest`` () = 
        //raise (Exception()) |> ignore
        (fun () -> raise (Exception())) |> should throw typeof<Exception>
    
    [<Test>] [<Category("Tag2")>] [<Platform("Win98, WinME")>]
    let ``skippedWinMETest`` () = ignore () // ...
    
    [<Test>] [<Category("Tag1")>] //[<Timeout(100)>] 
    let ``passedTest`` () = ignore () // Assert.Pass()
    
    [<Test>] [<ExpectedException("System.InvalidOperationException")>]
    let ``expectAnException`` () = 
        //raise (InvalidOperationException()) |> ignore
        raise (InvalidOperationException()) |> 
            should throw typeof<InvalidOperationException>

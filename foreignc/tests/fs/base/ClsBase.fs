namespace Base

open System
open NUnit.Framework

module Misc =
    let getdisposable fn = 
        {new IDisposable with
             member x.Dispose () = fn ()}

[<TestFixture>]
type ClsBase () =
    member this.d = Misc.getdisposable (fun () ->
        printf "Base Dispose(%A)\n" <| this.GetType())
    interface System.IDisposable with
        member this.Dispose () = this.d.Dispose ()
    
    [<TestFixtureSetUp>]
    abstract SetUpClass : unit -> unit
    default this.SetUpClass () = 
        printf "Base SetUpClass(%A)\n" <| this.GetType().BaseType
    
    [<TestFixtureTearDown>]
    abstract TearDownClass : unit -> unit
    default this.TearDownClass () = 
        printf "Base TearDownClass(%A)\n" <| this.GetType().BaseType
    
    [<SetUp>]
    abstract SetUp : unit -> unit
    default this.SetUp () = 
        printf "Base SetUp(%A)\n" <| this.GetType().BaseType
    
    [<TearDown>]
    abstract TearDown : unit -> unit
    default this.TearDown () = 
        printf "Base TearDown(%A)\n" <| this.GetType().BaseType
    
    member this.WrapTest(startFun, endFun, testFun) =
        startFun () ; testFun () ; endFun ()

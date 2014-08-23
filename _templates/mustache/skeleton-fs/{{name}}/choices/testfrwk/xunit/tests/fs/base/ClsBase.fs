namespace Base

open System
open System.IO
open Xunit

module Misc =
    let getdisposable fn = 
        {new IDisposable with
             member x.Dispose () = fn ()}

type ClsFixture () =
    [<DefaultValue>] val mutable output : System.IO.StreamWriter
    
    (*do
        this.output <- File.AppendText("testout.txt")
        fprintfn this.output "Base SetUpClass(%A)" <| this.GetType().BaseType*)
    
    member this.d = Misc.getdisposable (fun () ->
        (*fprintfn this.output "Base TearDownClass(%A)" <| this.GetType().BaseType ;
        fprintfn this.output "%s" <| String.replicate 40 "-" ;
        this.output.Close () ;*)
        ())
    interface System.IDisposable with
        member this.Dispose () = this.d.Dispose ()

type ClsBase () =
    inherit ClsFixture ()
    
    member this.d = Misc.getdisposable (fun () ->
        (*fprintfn this.output "Base Dispose(%A)" <| this.GetType() ;*)
        ())
    interface System.IDisposable with
        member this.Dispose () = this.d.Dispose () ; base.d.Dispose ()
    
    abstract SetUp : unit -> unit
    default this.SetUp () = 
        (*fprintfn this.output "Base SetUp(%A)" <| this.GetType().BaseType ;*)
        ()
    
    abstract TearDown : unit -> unit
    default this.TearDown () = 
        (*fprintfn this.output "Base TearDown(%A)" <| this.GetType().BaseType ;*)
        ()
    
    member this.WrapTest(startFun, endFun, testFun) =
        startFun () ; testFun () ; endFun ()

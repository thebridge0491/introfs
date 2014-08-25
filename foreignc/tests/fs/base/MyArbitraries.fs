namespace Base
    
open System
open FsCheck

type MyArbitraries =
    //FsCheck.NonEmptyString
    static member Strings () = 
        (*Arb.Default.String().Generator |> Gen.suchThat (fun s -> not <| String.IsNullOrEmpty s)
            |> Arb.fromGen*)
        Arb.Default.String () |> Arb.filter (fun s -> 
            not <| String.IsNullOrEmpty s)
    
    //FsCheck.NormalFloat
    static member Doubles () =
        (*Arb.Default.Float().Generator |> Gen.suchThat (fun n -> (not <| Double.IsNaN n) && (not <| Double.IsInfinity n))
            |> Arb.fromGen*)
        Arb.Default.Float () |> Arb.filter (fun n -> 
            (not <| Double.IsNaN n) && (not <| Double.IsInfinity n))
    
    //FsCheck.NonEmptyArray
    static member DoubleArr () =
        (*Arb.Default.Array<float>().Generator |> Gen.suchThat (fun xs -> xs.Length <> 0)
            |> Arb.fromGen*)
        Arb.Default.Array<float> () |> Arb.filter (fun xs ->
            xs.Length <> 0)
    
    static member Enumerables<'T> () = 
        (*Arb.Default.Array<'T>().Generator |> Gen.suchThat (fun xs ->
            xs.Length <> 0) |> Arb.fromGen |> Arb.convert Array.toSeq Seq.toArray*)
        Arb.Default.Array<'T> () |> Arb.filter (fun xs ->
            xs.Length <> 0) |> Arb.convert Array.toSeq Seq.toArray

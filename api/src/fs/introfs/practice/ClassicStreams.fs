#light (*
exec fsharpi /nologo /warn:3 /checked /lib:.,$HOME/nuget/packages `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs fsharp.core log4net` /r:Introfs.Practice.dll $0 $@
*)

namespace Introfs.Practice

//#r "Introfs.Util.dll" ;;
//#r "Introfs.Practice.dll" ;;

module ClassicStreams = 

    //open System
    
    let rec squaresMap2 () =
        Seq.append [0.0f] (Seq.delay (fun () ->
            Seq.map2 (fun _ e2 -> e2 * e2) (squaresMap2 ())
                (Seq.initInfinite (fun i -> (float32 i) + 1.0f))))
    
    let rec exptsMap2 b =
        Seq.append [1.0f] (Seq.delay (fun () ->
            Seq.map2 (fun _ e2 -> b ** e2) (exptsMap2 b)
                (Seq.initInfinite (fun i -> (float32 i) + 1.0f))))
    
    let rec sumsMap2 lo =
        Seq.append [lo] (Seq.delay (fun () ->
            Seq.map2 (fun e1 e2 -> e1 + e2 + lo) (sumsMap2 lo)
                (Seq.initInfinite (fun i -> (int64 i) + 1L))))
    
    let rec factsMap2 () =
        Seq.append [1L] (Seq.delay (fun () ->
            Seq.map2 ( * ) (factsMap2 ()) 
                (Seq.initInfinite (fun i -> (int64 i) + 1L))))
    
    let rec fibsMap2 () =
        Seq.append [0; 1] (Seq.delay (fun () ->
            Seq.map2 (+) (fibsMap2 ()) (Seq.tail (fibsMap2 ()))))
    
    let rec pascalrowsMap2 () =
        Seq.append [[1]] (Seq.delay (fun () ->
            Seq.map2 (fun e1 _ -> List.map2 (+) (0 :: e1) (e1 @ [0])) 
                (pascalrowsMap2 ()) (Seq.initInfinite id)))
    
    
    let squaresU () = Seq.unfold (fun z -> Some (z * z, z + 1.0f)) 0.0f
    
    let exptsU b = Seq.unfold (fun z -> Some (z, z * b)) 1.0f
    
    let sumsU lo = Seq.unfold (fun (z, ct) -> 
        Some (z, (z + (int64 ct) + lo, ct + 1))) (lo, 1)
    
    let factsU () = Seq.unfold (fun (z, ct) -> 
        Some (z, (z * (int64 ct), ct + 1))) (1L, 1)
    
    let fibsU () = Seq.unfold (fun (s0, s1) -> 
        Some (s0, (s1, s0 + s1))) (0, 1)
    
    let pascalrowsU () = Seq.unfold (fun row -> 
        Some (row, List.map2 (+) (0 :: row) (row @ [0]))) [1]
    
    
    let squaresLc () =
        let rec iter z =
            Seq.append [z * z] (Seq.delay (fun () -> iter (z + 1.0f))) in
        iter 0.0f
    
    let exptsLc (b:float32) =
        let rec iter z =
            Seq.append [b ** z] (Seq.delay (fun () -> iter (z + 1.0f))) in
        iter 0.0f
    
    let rec sumsLc lo =
        Seq.append [lo] (Seq.delay (fun () ->
            Seq.map2 (fun a b -> a + b + lo) (sumsLc lo) 
                (Seq.initInfinite (fun i -> (int64 i) + 1L))))
    
    let rec factsLc () =
        Seq.append [1L] (Seq.delay (fun () ->
            Seq.map2 (fun a b -> a * b) (factsLc ()) 
                (Seq.initInfinite (fun i -> (int64 i) + 1L))))
    
    let rec fibsLc () =
        Seq.append [0; 1] (Seq.delay (fun () ->
            Seq.map2 (fun a b -> a + b) (fibsLc ()) (Seq.tail (fibsLc ()))))
    
    let rec pascalrowsLc () =
        Seq.append [[1]] (Seq.delay (fun () ->
            Seq.map (fun row -> List.map2 (+) (0 :: row) (row @ [0]))
                (pascalrowsLc ())))
    
    
    let squaresScanl () =
        Seq.delay (fun () -> Seq.scan (fun _ e -> e * e) 0.0f <|
            (Seq.initInfinite (fun i -> (float32 i) + 1.0f)))
    
    let rec exptsScanl b =
        Seq.append [1.0f] (Seq.delay (fun () -> 
            Seq.scan (fun a _ -> a * b) b (exptsScanl b)))
    
    let sumsScanl lo =
        Seq.delay (fun () -> Seq.scan (fun a e -> a + e + lo) lo <|
            (Seq.initInfinite (fun i -> (int64 i) + 1L)))
    
    let factsScanl () =
        Seq.delay (fun () -> Seq.scan ( * ) 1L <|
            (Seq.initInfinite (fun i -> (int64 i) + 1L)))
    
    let rec fibsScanl () =
        Seq.append [0] (Seq.delay (fun () -> 
            Seq.scan ( + ) 1 (fibsScanl ())))
    
    let rec pascalrowsScanl () =
        Seq.delay (fun () -> Seq.scan (fun _ row -> 
            List.map2 (+) (0 :: row) (row @ [0])) [1] (pascalrowsScanl ()))

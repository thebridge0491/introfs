#light (*
exec fsharpi /nologo /warn:3 /checked /lib:.,$HOME/nuget/packages `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs fsharp.core log4net` /r:Introfs.Practice.dll $0 $@
*)

namespace Introfs.Practice

//#r "Introfs.Util.dll" ;;
//#r "Introfs.Practice.dll" ;;

//open System

/// <summary>Classic (higher-order) module.</summary>
module ClassicHiorder =
    
    module Util = Introfs.Util.Library
    
    let log = log4net.LogManager.GetLogger("prac")

    let exptF b n = List.fold (fun a _ -> a * b) 1.0f [1 .. (int n)]
    
    let squareF n = exptF n 2.0f
    
    let numseqMathF init op hi lo =
        match hi >= lo with
        |   true -> List.fold op init [lo .. hi]
        |   _ -> init
    
    let sumToF hi lo = numseqMathF 0L (+) hi lo
    
    let factF n = numseqMathF 1L ( * ) n 1L
    
    let fibF n = snd <| List.fold (fun (s0, s1) _ -> 
            (s0 + s1, s0)) (0, 1) [0 .. n]
    
    let pascaltriF rows = 
        List.rev <| List.fold (fun acc _ -> 
            match acc with
            |   x::xs -> (List.map2 (+) (0::x) (x @ [0])) :: acc
            |   _ -> raise (Failure "erroneous input")) [[1]] [1 .. rows]
    
    
    let _euclid m n = 
        let rec iter a b =
            match b with
            |   0 -> a
            |   _ -> iter b (a % b) in
        iter m n
    
    let gcdF (nss:int list) =
        match nss with
        |   [] -> 0
        |   x::xs -> List.fold _euclid x xs
    
    let lcmF (nss:int list) =
        match nss with
        |   [] -> 0
        |   x::xs -> List.fold (fun a b -> a * b / (_euclid a b)) x xs

    let baseExpandF b n = 
        fst (List.fold (fun (acc, num) _ ->
            match num with
            |   0 -> (acc, num / b)
            |   _ -> ((num % b) :: acc, num / b)) ([], n) <|
                [0 .. ((int <| (Operators.log (float n)) / (Operators.log (float b))) + 1)])

    let baseTo10F b (nss:int list) = 
        let proc e (h, t) =
            (h + 1, t + (e * (int <| (float b ** float h)))) in
        snd (List.foldBack proc nss (0, 0))

    let rangeStepF step start stop = 
        let cmpOp = if step > 0 then (>) else (<) in
        let cnt =
            match step > 0 with
            |   true -> (abs (stop - start)) + 1
            |   _ -> (abs (start - stop)) + 1 in
        List.rev <| List.fold (fun acc e ->
            match cmpOp ((e * step) + start) stop with
            |   true -> acc
            |   _ -> ((e * step) + start) :: acc) [] [0 .. (cnt - 1)]

    let rangeF start stop = rangeStepF 1 start stop


    let exptU b n =
        let res = List.rev (List.unfold (fun (h, t) ->
            match 0.0f >= t with
            |   true -> None
            |   _ -> Some (h * b, (h * b, t - 1.0f))) (1.0f, n)) in
        match res with | [] -> b | x::_ -> x
    
    let squareU n = exptU n 2.0f
    
    let numseqMathU init op hi lo = 
        let res = List.rev (List.unfold (fun (h, t) ->
            match t > (hi + 1L) with
            |   true -> None
            |   _ -> Some (h, (op h t, t + 1L))) (init, lo)) in
        match res with | [] -> init | x::_ -> x
    
    let sumToU hi lo = numseqMathU 0L (+) hi lo
    
    let factU n = numseqMathU 1L ( * ) n 1L
    
    let fibU n = 
        let res = List.rev (List.unfold (fun (s0, s1,m) ->
            match m with
            |   0 -> None
            |   _ -> Some (s1, (s1, s0 + s1, m - 1))) (0, 1, n)) in
        match res with | [] -> n | x::_ -> x
    
    let pascaltriU rows = 
        let res = List.rev (List.unfold (fun (acc, r) ->
            match (acc, 0 > r) with
            |   (x::xs, false) ->
                Some (acc, ((List.map2 (+) (0::x) (x @ [0])) :: acc, r - 1))
            |   (_, _) -> None) ([[1]], rows)) in
        match res with | [] -> [] | x::_ -> List.rev x
    
    (*
    let _euclidU m n =
        let res = List.rev (List.unfold (fun (a, b) ->
            match b with
            |   0 -> None
            |   _ -> Some (b, (b, a % b))) (m, n)) in
        match res with | [] -> m | x::_ -> x
    *)
    
    let gcdU (nss:int list) =
        match nss with
        |   [] -> 0
        |   m::ms ->
            let res = List.rev (List.unfold (fun (a, rst) ->
                match rst with
                |   [] -> None
                |   b::bs -> Some (_euclid a b, (_euclid a b, bs))) (m, ms)) in
            match res with | [] -> m | x::_ -> x
    
    let lcmU (nss:int list) =
        match nss with
        |   [] -> 0
        |   m::ms ->
            let res = List.rev (List.unfold (fun (a, rst) ->
                match rst with
                |   [] -> None
                |   b::bs -> 
                    Some (a * b / (_euclid a b), 
                        (a * b / (_euclid a b), bs))) (m, ms)) in
            match res with | [] -> m | x::_ -> x

    let baseExpandU b n = 
        List.rev (List.unfold (fun x ->
            match x with
            |   0 -> None
            |   _ -> Some (x % b, x / b)) n)

    let baseTo10U b (nss:int list) =
        let func = (fun (h, t) -> 
            match t with
            |   [] -> h
            |   x::xs ->
                (h + (x * (int (float b ** float (List.length xs)))))) in
        let res = List.rev (List.unfold (fun (h, t) ->
            match t with
            |   [] -> None
            |   _::xs -> Some (func (h, t), (func (h, t), xs))) (0, nss)) in
        match res with | [] -> 0 | x::_ -> x

    let rangeStepU step start stop = 
        let cmpOp = if step > 0 then (>) else (<) in
        List.unfold (fun x ->
            match cmpOp x stop with
            |   true -> None
            |   _ -> Some (x, x + step)) start

    let rangeU start stop = rangeStepU 1 start stop


    let exptLc b n = Util.headOr 1.0f <| 
        List.rev [for x in [0.0f .. n] do yield b ** x]
    
    let squareLc n = exptLc n 2.0f
    
    let numseqMathLc init op hi lo =
        let rec _helper () =
            Seq.append (seq [init]) (seq {for (a, b) in 
                Seq.zip (_helper ()) (seq {lo .. hi}) do yield op a b}) in
        Seq.item 0 (Seq.rev (_helper ()))
    
    let sumToLc hi lo = numseqMathLc 0L (+) hi lo
    
    let factLc n = numseqMathLc 1L ( * ) n 1L
    
    let fibLc n = 
        let rec _helper m = 
            match m with
            |   0 -> seq [0]
            |   _ ->
                Seq.append (seq [0; 1]) (seq {for (_, a, b) in
                    Seq.zip3 (seq {2 .. n}) (_helper m) 
                    (Seq.tail <| _helper m) do yield a + b})
        Seq.item 0 (Seq.rev (_helper n))
    
    let pascaltriLc rows =
        let rec _helper () = 
            Seq.append (seq [[1]]) (seq {for (_, row) in 
                Seq.zip (seq {1 .. rows}) (_helper ()) do
                yield List.map2 (+) ([0] @ row) (row @ [0])}) in
        Seq.toList (_helper ())

    let baseExpandLc b n = 
        [for m in List.rev [for i in 
            [0 .. (int (Operators.log (float n) / Operators.log (float b)))]
                do yield n / (int (float b ** float i))] do yield m % b]

    let baseTo10Lc b (nss:int list) =
        List.fold (+) 0 [for (i, x) in List.zip [0 .. (List.length nss) - 1] 
            (List.rev nss) do yield x * (int (float b ** float i))]
    

    /// <summary>Lib main function.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    //[<EntryPoint>]
    let libmain (args: string[]) = 
        let n = 5L in
        printfn "fact %d: %d" n (factF n)
        0

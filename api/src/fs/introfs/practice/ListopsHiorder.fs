#light (*
exec fsharpi /nologo /warn:3 /checked /lib:build/bin/Debug,build/bin/Release `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs ini-parser` /r:Introfs.Practice.dll $0 $@
*)

namespace Introfs.Practice

//#r "Introfs.Util.dll" ;;
//#r "Introfs.Practice.dll" ;;

//open System

/// <summary>Listops (higher-order) module.</summary>
module ListopsHiorder =
    
    module Util = Introfs.Util.Library
    
    let log = log4net.LogManager.GetLogger "prac" 
    
    let tabulateF func cnt =
        List.rev <| List.fold (fun a i -> func i :: a) [] [0 .. (cnt - 1)]
    
    let lengthF xss = List.fold (fun acc _ -> acc + 1) 0 xss
    
    let nthF idx xss = 
        snd (List.fold (fun (h, t) e -> 
            (h + 1, if idx = h then Some e else t)) (0, None) xss)
    
    let indexFindF ndx pred xs = 
        let corp (ndx1, (idx, it)) el =
            match pred el && it = None with
            |   true -> (ndx1 + 1, (Some ndx1, Some el))
            |   _ -> (ndx1 + 1, (idx, it)) in
        snd (List.fold corp (ndx, (None, None)) xs)
    
    let findIndexF pred xs = fst <| indexFindF 0 pred xs
    
    let findF pred xs = snd <| indexFindF 0 pred xs
    
    let minmaxF xss = 
        match xss with
        |   [] -> raise (Failure "empty list")
        |   x::xs -> List.fold (fun (lo, hi) e ->
                match (e < lo, e > hi) with
                |   (true, _) -> (e, hi)
                |   (_, true) -> (lo, e)
                |   (_, _) -> (lo, hi)) (x, x) xss
    
    let minF xss = fst <| minmaxF xss
    let maxF xss = snd <| minmaxF xss

    let revF xs = List.fold (fun acc el -> el :: acc) [] xs

    let copyF xss = List.foldBack (fun el acc -> el :: acc) xss []
    
    let splitF n xss =
        List.fold (fun (t, ys) _ ->
            match ys with
            |   [] -> (t, ys)
            |   z::zs -> (z :: t, zs)) ([], xss) [0 .. (n - 1)]
    
    let takeF n xss = List.rev (fst <| splitF n xss)
    let dropF n xss = snd <| splitF n xss
    
    let existsForallF pred xss = 
        List.fold (fun (m, n) e -> (m || pred e, n && pred e)) (false, true)
            xss

    let existsF pred xss = fst <| existsForallF pred xss
    let forallF pred xss = snd <| existsForallF pred xss

    let mapF proc xss = List.foldBack (fun e a -> proc e :: a) xss []

    let iterF proc xss = List.fold (fun _ e -> proc e) () xss
    
    let partitionF pred xss = 
        List.fold (fun (f, r) e -> 
            if pred e then (e :: f, r) else (f, e :: r)) ([], []) (List.rev xss)

    let filterF pred xss = fst <| partitionF pred xss
    let removeF pred xss = snd <| partitionF pred xss

    let isOrderedF cmpfn keyfn xss = 
        let corp = (fun (old, a) e -> (e, a && cmpfn (keyfn old) (keyfn e))) in
        match xss with
        |   [] -> true
        |   x::xs -> snd (List.fold corp (x, true) xs)
    
    
    let appendF xss yss = List.foldBack (fun e acc -> e :: acc) xss yss
    
    let interleaveF xss yss = 
        let lenShort =
            match List.length xss < List.length yss with
            | true -> List.length xss | _ -> List.length yss in
        fst (List.foldBack (fun e (a, rst) -> 
            match rst with
            |   [] -> (a, rst)
            |   z::zs -> (z :: e :: a, zs))
            (List.take lenShort yss) 
            ((List.skip lenShort xss) @ (List.skip lenShort yss),
                List.rev (List.take lenShort xss)))

    let map2F proc xss yss = 
        let lenShort =
            match List.length xss < List.length yss with
            | true -> List.length xss | _ -> List.length yss in
        List.rev (fst (List.fold (fun (a, (wss, zss)) _ ->
            match (wss, zss) with
            |   ([], _) -> (a, (wss, zss))
            |   (_, []) -> (a, (wss, zss))
            |   (w::ws, z::zs) -> (proc w z :: a, (ws, zs))
            ) ([], (xss, yss)) [0 .. (lenShort - 1)]))
    
    let zipF xss yss = map2F (fun a b -> (a, b)) xss yss
        
    let unzip2F xss = 
        List.foldBack (fun (eh, et) (ah, at) -> (eh :: ah, et :: at)) xss
            ([], [])

    let concatF nlsts = List.foldBack (fun e a -> e @ a) nlsts []


    let tabulateU func cnt = 
        let res = List.rev (List.unfold (fun (i, a) ->
            match cnt <= i with
            |   true -> None
            |   _ -> Some (func i :: a, (i + 1, func i :: a))) (0, [])) in
        match res with | [] -> [] | x::_ -> List.rev x
    
    let lengthU xss = 
        let res = List.rev (List.unfold (fun (h, t) ->
            match t with
            |   [] -> None
            |   _::xs -> Some (h + 1, (h + 1, xs))) (0, xss)) in
        match res with | [] -> 0 | x::_ -> x
    
    let nthU idx xss = 
        let res0 = List.rev (List.unfold (fun (i, e, l) ->
            match l with
            |   [] -> None
            |   x::xs -> 
                let res1 = if idx = i then Some x else e in
                Some (res1, (i + 1, res1, xs))) (0, None, xss)) in
        match res0 with | [] -> None | x::_ -> x
    
    let indexFindU ndx pred xs = 
        let fn z (idx, el) n = 
            match pred z && el = None with
            | true -> (Some n, Some z) | _ -> (idx, el) in
        let res = List.rev (List.unfold (fun (n, a, l) ->
            match l with
            |   [] -> None
            |   y::ys -> Some (fn y a n, (n + 1, fn y a n, ys))) (ndx, (None, None), xs)) in
        match res with | [] -> (None, None) | z::_ -> z
    
    let findIndexU pred xs = fst <| indexFindU 0 pred xs
    
    let findU pred xs = snd <| indexFindU 0 pred xs
    
    let minmaxU xss =
        match xss with
        |   [] -> raise (Failure "empty list")
        |   x::xs ->
            let fn z (lo, hi) =
                match (z < lo, z > hi) with
                | (true, _) -> (z, hi)
                | (_, true) -> (lo, z)
                | _ -> (lo, hi) in
            let res = List.rev (List.unfold (fun (a, yss) ->
                match yss with
                |   [] -> None
                |   y::ys -> Some (fn y a, (fn y a, ys))) ((x, x), xs)) in
            match res with | [] -> (x, x) | w::_ -> w
    
    let minU xss = fst <| minmaxU xss
    let maxU xss = snd <| minmaxU xss

    let revU xs = 
        List.rev (List.unfold (fun t ->
            match t with
            | [] -> None | y::ys -> Some (y, ys)) xs)

    let copyU xss = 
        List.unfold (fun t ->
            match t with
            | [] -> None | y::ys -> Some (y, ys)) xss
    
    let splitU n xss = 
        let res = List.rev (List.unfold (fun (ct, (t, d)) ->
            match (ct, d) with
            |   (0, _) | (_, []) -> None
            |   (_, x::xs) -> Some ((x :: t, xs), (ct - 1, (x :: t, xs)))) (n, ([], xss))) in
        match res with | [] -> ([], xss) | x::_ -> x
    
    let takeU n xss = List.rev (fst <| splitU n xss)
    let dropU n xss = snd <| splitU n xss
    
    let existsForallU pred xss = 
        let res = List.rev (List.unfold (fun ((e, a), l) ->
            match l with
            |   [] -> None
            |   x::xs -> 
                let (resE, resA) = (e || pred x, a && pred x) in
                Some ((resE, resA), ((resE, resA), xs))) ((false, true), xss)) in
        match res with | [] -> (false, true) | x::_ -> x

    let existsU pred xss = fst <| existsForallU pred xss
    let forallU pred xss = snd <| existsForallU pred xss

    let mapU proc xss = 
        List.unfold (fun t -> 
            match t with
            | [] -> None | x::xs -> Some (proc x, xs)) xss

    let iterU proc xss = 
        let res = List.rev (List.unfold (fun t ->
            match t with
            |   [] -> None
            |   x::xs -> Some (proc x; (), xs)) xss) in
        match res with | [] -> () | x::_ -> x
    
    let partitionU pred xss = 
        let res = List.rev (List.unfold (fun ((f, r), t) ->
            match t with
            |   [] -> None
            |   x::xs -> 
                match pred x with
                |   true -> Some ((x :: f, r), ((x :: f, r), xs))
                |   _ -> Some ((f, x :: r), ((f, x :: r), xs)))
                (([], []), List.rev xss)) in
        match res with | [] -> ([], []) | x::_ -> x

    let filterU pred xss = fst <| partitionU pred xss
    let removeU pred xss = snd <| partitionU pred xss

    let isOrderedU cmpfn keyfn xss = 
        match xss with
        |   [] -> true
        |   x::xs ->
            let fn oldval el = cmpfn (keyfn oldval) (keyfn el) in
            let res = List.rev (List.unfold (fun (a, o, l) ->
                match l with
                |   [] -> None
                |   y::ys -> Some (a && fn o y, (a && fn o y, y, ys)))
                (true, x, xs)) in
            match res with | [] -> true | r::_ -> r
    
    
    let appendU xss yss = 
        let res = List.rev (List.unfold (fun (h, t) ->
            match t with
            |   [] -> None
            |   x::xs -> Some (x :: h, (x :: h, xs))) (yss, List.rev xss)) in
        match res with | [] -> yss | r::_ -> r
    
    let interleaveU xss yss =
        let res = List.rev (List.unfold (fun (wss, zss) ->
            match (wss, zss) with
            |   ([], []) -> None
            |   ([], z::zs) -> Some (z, (zs, []))
            |   (w::ws, _) -> Some (w, (zss, ws))) (xss, yss)) in
        match res with | [] -> yss | r -> List.rev r

    let map2U proc xss yss =
        let res = List.rev (List.unfold (fun (h, t) ->
            match (h, t) with
            |   ([], _) -> None
            |   (_, []) -> None
            |   (x::xs, y::ys) -> Some (proc x y, (xs, ys))) (xss, yss)) in
        match res with | [] -> [] | r -> List.rev r
    
    let zipU xss yss = map2U (fun a b -> (a, b)) xss yss
        
    let unzip2U xss = 
        let res = List.rev (List.unfold (fun ((h, t), l) ->
            match ((h, t), l) with
            |   ((_, _), []) -> None
            |   ((ws, zs), (w, z)::us) -> Some ((w :: ws, z :: zs), ((w :: ws, z :: zs), us))) (([], []), List.rev xss)) in
        match res with | [] -> ([], []) | r::_ -> r

    let concatU nlsts =
        let res = List.rev (List.unfold (fun (h, t) ->
            match t with
            |   [] -> None
            |   x::xs -> Some (x @ h, (x @ h, xs))) ([], List.rev nlsts)) in
        match res with | [] -> [] | r::_ -> r


    let tabulateLc func cnt = [for i in [0 .. (cnt - 1)] do yield func i]
    
    let nthLc idx xss = 
        let _helper = [for (i, e) in List.mapi (fun i e -> (i, e)) xss do
            if idx = i then yield Some e] in
        Util.headOr None _helper
    
    let indexFindLc ndx pred xs = 
        let _helper = [for (i, e) in List.mapi (fun i e -> (i, e)) (List.skip ndx xs) do if pred e then yield (Some i, Some e)] in
        Util.headOr (None, None) _helper
    
    let findIndexLc pred xs = fst <| indexFindLc 0 pred xs
    
    let findLc pred xs = snd <| indexFindLc 0 pred xs

    let copyLc xss = [for e in xss do yield e]
    
    let takeLc n xss =
        [for (i, e) in List.mapi (fun i e -> (i, e)) xss do
            if n > i then yield e]
    
    let dropLc n xss =
        [for (i, e) in List.mapi (fun i e -> (i, e)) xss do
            if not (n > i) then yield e]

    let mapLc proc xss = [for e in xss do yield proc e]

    let iterLc proc xss = Util.headOr () [for e in xss do yield proc e]

    let filterLc pred xss = [for e in xss do if pred e then yield e]
    
    let removeLc pred xss = [for e in xss do if not (pred e) then yield e]

    let isOrderedLc cmpfn keyfn xss = 
        let yss = 
            match xss with
            | [] -> xss | x::[] -> xss
            | _ -> (List.tail xss) @ (List.take 1 (List.rev xss)) in
        List.forall (fun e -> true = e) [for (a, b) in List.zip xss yss do
            yield cmpfn (keyfn a) (keyfn b)]
    
    
    let interleaveLc xss yss = 
        let lenShort =
            match List.length xss < List.length yss with
            | true -> List.length xss | _ -> List.length yss in
        (List.concat [for (a, b) in
            List.zip (List.take lenShort xss) (List.take lenShort yss) do 
                yield [a; b]]) @ ((List.skip lenShort xss) @ (List.skip lenShort yss))

    let map2Lc proc xss yss = 
        let lenShort =
            match List.length xss < List.length yss with
            | true -> List.length xss | _ -> List.length yss in
        [for (a, b) in 
            List.zip (List.take lenShort xss) (List.take lenShort yss) do 
                yield proc a b]
    
    let zipLc xss yss = map2Lc (fun a b -> (a, b)) xss yss


    /// <summary>Lib main function.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    //[<EntryPoint>]
    let libmain (args: string[]) = 
        let (n, xs) = (3, [2; 1; 0; 4; 3]) in
        printfn "indexOf %d %s: %d" n (xs.ToString ())
            (Option.defaultValue -1 <| findIndexF (fun e -> n = e) xs)
        0

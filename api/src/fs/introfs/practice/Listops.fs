#light (*
exec fsharpi /nologo /warn:3 /checked /lib:.,$HOME/nuget/packages `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs fsharp.core log4net` /r:Introfs.Practice.dll $0 $@
*)

namespace Introfs.Practice

//#r "Introfs.Util.dll" ;;
//#r "Introfs.Practice.dll" ;;

//open System

/// <summary>Listops module.</summary>
module Listops =

    let log = log4net.LogManager.GetLogger "prac" 
    
    let tabulateI func cnt =
        let rec iter idx acc =
            match (1 > idx) with
            | true -> acc
            | _ -> iter (idx - 1) ((func (idx - 1)) :: acc) in
        iter cnt []

    let rec tabulateR func cnt =
        match (1 > cnt) with
        | true -> []
        | _ -> (tabulateR func (cnt - 1)) @ [func (cnt - 1)]
    
    let lengthI xss = 
        let rec iter acc rst =
            match rst with
            |   [] -> acc
            |   _::ys -> iter (acc + 1) ys in
        iter 0 xss

    let rec lengthR xss =
        match xss with
        |   [] -> 0 
        |   _::ys -> 1 + (lengthR ys)
    
    let nthI idx xss = 
        let rec iter ndx rst =
            match rst with
            |   [] -> None
            |   y::ys ->
                match ndx with
                |   0 -> Some y
                |   _ -> iter (ndx - 1) ys in
        iter idx xss

    let rec nthR idx xss =
        match xss with
        |   [] -> None
        |   y::ys ->
            match idx with
            |   0 -> Some y
            |   _ -> nthR (idx - 1) ys
    
    let indexFindI ndx pred xs =
        let rec iter idx rst = 
            match rst with
            |   [] -> (None, None)
            |   y::ys -> 
                match pred y with
                |   true -> (Some idx, Some y)
                |   _ -> iter (idx + 1) ys in
        iter ndx xs
    
    let rec indexFindR ndx pred xs =
        match xs with
        |   [] -> (None, None)
        |   y::ys ->
            match pred y with
            |   true -> (Some ndx, Some y)
            |   _ -> indexFindR (ndx + 1) pred ys
    
    (*let findIndexI pred xs = Sequenceops.findIndexI pred (Seq.toList xs)*)
    (*let findIndexI pred xs =
        let rec iter idx rst = 
            match rst with
            |   [] -> -1
            |   y::ys -> 
                match pred y with
                |   true -> idx
                |   _ -> iter (idx + 1) ys
        in iter 0 xs*)
    
    let findIndexI pred xs = fst <| indexFindI 0 pred xs
    let findIndexR pred xs = fst <| indexFindR 0 pred xs
    
    let findI pred xs = snd <| indexFindI 0 pred xs
    let findR pred xs = snd <| indexFindR 0 pred xs
    
    let minmaxI xss =
        match xss with
        |   [] -> raise (Failure "empty list")
        |   z::zs ->
            let rec iter (lo, hi) rst =
                match rst with
                |   [] -> (lo, hi)
                |   y::ys ->
                    match (y < lo, y > hi) with
                    |   (true, _) -> iter (y, hi) ys
                    |   (_, true) -> iter (lo, y) ys
                    |   (_, _) -> iter (lo, hi) ys in
            iter (z, z) zs
    
    let minmaxR xss =
        let rec _helper norm rst =
            match rst with
            |   [] -> raise (Failure "empty list")
            |   y::[] -> y
            |   y::z::zs ->
                match (norm (y < z)) with
                |   true -> _helper norm ([y] @ zs)
                |   _ -> _helper norm ([z] @ zs) in
        (_helper id xss, _helper not xss)
    
    let minI xss = fst <| minmaxI xss
    let minR xss = fst <| minmaxR xss
    
    let maxI xss = snd <| minmaxI xss
    let maxR xss = snd <| minmaxR xss

    let rec revR xs = 
        match xs with
        |   [] -> []
        |   y::ys -> (revR ys) @ [y]

    let revI xs =
        log.Info "revI()" ;
        let rec iter rst acc = 
            match rst with
            |   [] -> acc
            |   y::ys -> iter ys (y::acc)
        in iter xs []
    
    let copyI xss =
        let rec iter rst acc = 
            match rst with
            |   [] -> acc
            |   y::ys -> iter ys ([y] @ acc) in
        iter (List.rev xss) []
    
    let rec copyR xss = 
        match xss with
        |   [] -> []
        |   y::ys -> [y] @ (copyR ys)
    
    let splitI n xss =
        let rec iter m rst acc =
            match rst with
            |   [] -> (List.rev acc, [])
            |   y::ys ->
                match m with
                |   0 -> (List.rev acc, rst)
                |   _ -> iter (m - 1) ys ([y] @ acc) in
        iter n xss []
    
    let takeI n xss = fst <| splitI n xss
    let dropI n xss = snd <| splitI n xss
    
    let existsForallI pred xss = 
        let rec iter norm rst =
            match rst with
            |   [] -> norm false
            |   y::ys ->
                match norm (pred y) with
                |   true -> norm true
                |   _ -> iter norm ys in
        (iter id xss, iter not xss)

    let existsForallR pred xss = 
        let rec _helper norm rst =
            match rst with
            |   [] -> norm false
            |   y::ys ->
                match norm (pred y) with
                |   true -> norm true
                |   _ -> _helper norm ys in
        (_helper id xss, _helper not xss)

    let existsI pred xss = fst <| existsForallI pred xss
    let existsR pred xss = fst <| existsForallR pred xss

    let forallI pred xss = snd <| existsForallI pred xss
    let forallR pred xss = snd <| existsForallR pred xss

    let mapI proc xss = 
        let rec iter rst acc =
            match rst with
            |   [] -> acc
            |   y::ys -> iter ys ([(proc y)] @ acc) in
        iter (List.rev xss) []

    let rec mapR proc xss = 
        match xss with
        |   [] -> []
        |   y::ys -> [(proc y)] @ (mapR proc ys)

    let iterI proc xss = 
        let rec iter rst =
            match rst with
            |   [] -> ()
            |   y::ys -> proc y ; iter ys in
        iter xss

    let rec iterR proc xss = 
        match xss with
        |   [] -> ()
        |   y::ys ->  proc y ; iterR proc ys
    
    let partitionI pred xss = 
        let rec iter rst acc =
            match rst with
            |   [] -> acc
            |   y::ys ->
                match pred y with
                |   true -> iter ys ([y] @ (fst acc), snd acc)
                |   _ -> iter ys (fst acc, [y] @ (snd acc)) in
        iter (List.rev xss) ([], [])

    let partitionR pred xss = 
        let rec _helper norm rst =
            match rst with
            |   [] -> []
            |   y::ys ->
                match norm (pred y) with
                |   true -> [y] @ (_helper norm ys)
                |   _ -> _helper norm ys in
        (_helper id xss, _helper not xss)

    let filterI pred xss = fst <| partitionI pred xss
    let filterR pred xss = fst <| partitionR pred xss

    let removeI pred xss = snd <| partitionI pred xss
    let removeR pred xss = snd <| partitionR pred xss

    let foldLeftI corp init xss = 
        let rec iter acc rst =
            match rst with
            |   [] -> acc
            |   y::ys -> iter (corp acc y) ys in
        iter init xss

    let rec foldLeftR corp init xss =
        match xss with
        |   [] -> init
        |   y::ys -> foldLeftR corp (corp init y) ys

    let foldRightI proc xss init = 
        let rec iter rst acc =
            match rst with
            |   [] -> acc
            |   y::ys -> iter ys (proc y acc) in
        iter (List.rev xss) init

    let rec foldRightR proc xss init =
        match xss with
        |   [] -> init
        |   y::ys -> proc y (foldRightR proc ys init)

    let unfoldRightI func seed =
        let rec iter cur acc =
            match func cur with
            |   None -> acc
            |   Some (a, newCur) -> iter newCur ([a] @ acc) in
        iter seed []

    let rec unfoldLeftR func seed =
        match func seed with
        |   None -> []
        |   Some (a, newSeed) -> [a] @ (unfoldLeftR func newSeed)

    let isOrderedI cmpfn keyfn xss =
        match xss with
        |   [] -> true
        |   y::ys ->
            let rec iter rst oldval acc =
                match rst with
                |   [] -> acc
                |   z::zs -> 
                    iter zs z ((cmpfn (keyfn oldval) (keyfn z)) && acc) in
            iter ys y true

    let rec isOrderedR cmpfn keyfn xss =
        match xss with
        |   [] -> true
        |   _::[] -> true
        |   y::z::zs -> 
            (cmpfn (keyfn y) (keyfn z)) && 
                (isOrderedR cmpfn keyfn ([z] @ zs))
    
    
    let appendI xss yss =
        let rec iter rst acc = 
            match rst with
            |   [] -> acc
            |   y::ys -> iter ys ([y] @ acc) in
        iter (List.rev xss) yss
    
    let rec appendR xss yss =
        match xss with
        |   [] -> yss
        |   x::xs -> [x] @ (appendR xs yss)
    
    let interleaveI xss yss =
        let rec iter rst1 rst2 acc = 
            match (rst1, rst2) with
            |   ([], _) -> (List.rev acc) @ rst2
            |   (_, []) -> (List.rev acc) @ rst1
            |   (x::xs, y::ys) -> iter xs ys ([y; x] @ acc) in
        iter xss yss []
    
    let rec interleaveR xss yss =
        match (xss, yss) with
        |   ([], _) -> yss
        |   (_, []) -> xss
        |   (x::xs, y::ys) -> [x] @ (interleaveR yss xs)

    let map2I proc xss yss =
        let rec iter wss zss acc =
            match (wss, zss) with
            |   ([], _) -> acc
            |   (_, []) -> acc
            |   (w::ws, z::zs) -> iter ws zs (acc @ [proc w z]) in
        iter xss yss []

    let rec map2R proc xss yss =
        match (xss, yss) with
        |   ([], _) -> []
        |   (_, []) -> []
        |   (x::xs, y::ys) -> [proc x y] @ (map2R proc xs ys)
    
    let zipI xss yss = map2I (fun a b -> (a, b)) xss yss
    let zipR xss yss = map2R (fun a b -> (a, b)) xss yss
        
    let unzip2I xss =
        let rec iter rst acc =
            match rst with
            |   [] -> acc
            |   x::xs -> iter xs (fst x :: fst acc, snd x :: snd acc) in
        iter (List.rev xss) ([], [])

    let concatI nlsts =
        match nlsts with
        |   [] -> []
        |   z::zs ->
            let rec iter acc rst =
                match rst with
                |   [] -> List.rev acc
                |   x::xs -> iter ((List.rev x) @ acc) xs in 
            iter (List.rev z) zs

    let rec concatR nlsts =
        match nlsts with
        |   [] -> []
        |   z::zs -> z @ (concatR zs)


    /// <summary>Lib main function.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    //[<EntryPoint>]
    let libmain (args: string[]) = 
        let (n, xs) = (3, [2; 1; 0; 4; 3]) in
        printfn "indexOf %d %s: %d" n (xs.ToString ())
            (Option.defaultValue -1 <| findIndexI (fun e -> n = e) xs)
        0

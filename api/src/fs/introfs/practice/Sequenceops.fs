#light (*
exec fsharpi /nologo /warn:3 /checked /lib:.,$HOME/nuget/packages `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs fsharp.core log4net` /r:Introfs.Practice.dll $0 $@
*)

namespace Introfs.Practice

//#r "Introfs.Util.dll" ;;
//#r "Introfs.Practice.dll" ;;

//open System

/// <summary>Sequenceops module.</summary>
module Sequenceops =
    
    let log = log4net.LogManager.GetLogger("prac")
    
    let tabulateI func cnt =
        let rec iter idx acc =
            match (1 > idx) with
            | true -> acc
            | _ -> iter (idx - 1) (Seq.append [func (idx - 1)] acc) in
        iter cnt Seq.empty

    let rec tabulateR func cnt =
        match (1 > cnt) with
        | true -> Seq.empty
        | _ -> Seq.append (tabulateR func (cnt - 1)) [func (cnt - 1)]
    
    let lengthI coll = 
        let rec iter acc rst =
            match Seq.isEmpty rst with
            |   true -> acc
            |   _ -> iter (acc + 1) (Seq.tail rst) in
        iter 0 coll

    let rec lengthR coll =
        match Seq.isEmpty coll with
        |   true -> 0 
        |   _ -> 1 + (lengthR <| Seq.tail coll)
    
    let nthI idx coll = 
        let rec iter ndx rst =
            match Seq.isEmpty rst with
            |   true -> None
            |   _ ->
                match ndx with
                |   0 -> Some (Seq.head rst)
                |   _ -> iter (ndx - 1) (Seq.tail rst) in
        iter idx coll

    let rec nthR idx coll =
        match Seq.isEmpty coll with
        |   true -> None
        |   _ ->
            match idx with
            |   0 -> Some (Seq.head coll)
            |   _ -> nthR (idx - 1) (Seq.tail coll)
    
    let indexFindI ndx pred coll =
        let rec iter idx rst = 
            match Seq.length rst with
            |   0 -> (None, None)
            |   _ -> 
                match pred (Seq.head rst) with
                |   true -> (Some idx, Some (Seq.head rst))
                |   _ -> iter (idx + 1) (Seq.tail rst) in
        iter ndx coll
    
    let rec indexFindR ndx pred coll =
        match Seq.length coll with
        |   0 -> (None, None)
        |   _ ->
            match pred (Seq.head coll) with
            |   true -> (Some ndx, Some (Seq.head coll))
            |   _ -> indexFindR (ndx + 1) pred (Seq.tail coll)
    
    let findIndexI pred coll = 
        log.Info("findIndexI()") ; fst <| indexFindI 0 pred coll
    let findIndexR pred coll = fst <| indexFindR 0 pred coll
    
    let findI pred coll = snd <| indexFindI 0 pred coll
    let findR pred coll = snd <| indexFindR 0 pred coll
    
    (*let findIndexRszI pred cslst =
        let rec iter idx rst = 
            match rst.Count with
            |   0 -> -1
            |   _ -> 
                match pred (rst.Item 0) with
                |   true -> idx
                |   _ -> iter (idx + 1) (rst.GetRange (1, rst.Count - 1))
        in iter 0 cslst*)
    (*let findIndexRszI pred cslst = findIndexI pred cslst
        //findIndexI pred <| Seq.ofArray (cslst.ToArray () )
        //findIndexI pred <| Linq.Enumerable.AsEnumerable cslst*)
    
    let minmaxI coll =
        match Seq.isEmpty coll with
        |   true -> raise (Failure "empty seq")
        |   _ ->
            let rec iter (lo, hi) rst =
                match Seq.isEmpty rst with
                |   true -> (lo, hi)
                |   _ ->
                    let el = Seq.head rst in
                    match (el < lo, el > hi) with
                    |   (true, _) -> iter (el, hi) (Seq.tail rst)
                    |   (_, true) -> iter (lo, el) (Seq.tail rst)
                    |   (_, _) -> iter (lo, hi) (Seq.tail rst) in
            iter (Seq.head coll, Seq.head coll) (Seq.tail coll)
    
    let minmaxR coll =
        let rec _helper norm rst =
            match Seq.length rst with
            |   0 -> raise (Failure "empty seq")
            |   1 -> Seq.head rst
            |   _ ->
                let (x, y, zs) = (Seq.head rst, Seq.head (Seq.tail rst),
                        Seq.tail <| Seq.tail rst) in
                match (norm (x < y)) with
                |   true -> _helper norm (Seq.append (Seq.singleton x) zs)
                |   _ -> _helper norm (Seq.append (Seq.singleton y) zs) in
        (_helper id coll, _helper not coll)
    
    let minI coll = fst <| minmaxI coll
    let minR coll = fst <| minmaxR coll
    
    let maxI coll = snd <| minmaxI coll
    let maxR coll = snd <| minmaxR coll
    
    let revI coll =
        let rec iter rst acc = 
            match Seq.length rst with
            |   0 -> acc
            |   _ -> iter (Seq.tail rst) (Seq.append [Seq.head rst] acc) in
        iter coll Seq.empty
    
    let rec revR coll = 
        match Seq.length coll with
        |   0 -> Seq.empty
        |   _ -> Seq.append (revR <| Seq.tail coll) [Seq.head coll]
    
    (*let revRszI cslst =
        let rec iter rst acc = 
            match rst.Count with
            |   0 -> acc
            |   _ -> 
                let acc0 = ResizeArray<_> [rst.Item(0)] in
                acc0.AddRange (acc) ;
                iter (rst.GetRange (1, rst.Count - 1)) acc0
        in iter cslst (ResizeArray<_> () )*)
    (*let revRszI cslst = revI cslst //ResizeArray<_> (revI cslst)*)

    (*let rec revRszR cslst = 
        match cslst.Count with
        |   0 -> ResizeArray<_> ()
        |   _ -> 
            let ans = revRszR <| cslst.GetRange (1, cslst.Count - 1) in
            ans.Add (cslst.Item (0)) ; ans*)
    (*let revRszR cslst = revR cslst*)
    
    let copyI coll =
        let rec iter rst acc = 
            match Seq.isEmpty rst with
            |   true -> acc
            |   _ -> iter (Seq.tail rst) (Seq.append [Seq.head rst] acc) in
        iter (Seq.rev coll) Seq.empty
    
    let rec copyR coll = 
        match Seq.isEmpty coll with
        |   true -> Seq.empty
        |   _ -> Seq.append [Seq.head coll] (copyR <| Seq.tail coll)
    
    let splitI n coll =
        let rec iter m rst acc =
            match Seq.isEmpty rst with
            |   true -> (Seq.rev acc, Seq.empty)
            |   _ ->
                match m with
                |   0 -> (Seq.rev acc, rst)
                |   _ -> iter (m - 1) (Seq.tail rst) <|
                        (Seq.append [Seq.head rst] acc) in
        iter n coll Seq.empty
    
    let takeI n coll = fst <| splitI n coll
    let dropI n coll = snd <| splitI n coll
    
    let existsForallI pred coll = 
        let rec iter norm rst =
            match Seq.isEmpty rst with
            |   true -> norm false
            |   _ ->
                match norm (pred (Seq.head rst)) with
                |   true -> norm true
                |   _ -> iter norm (Seq.tail rst) in
        (iter id coll, iter not coll)

    let existsForallR pred coll = 
        let rec _helper norm rst =
            match Seq.isEmpty rst with
            |   true -> norm false
            |   _ ->
                match norm (pred (Seq.head rst)) with
                |   true -> norm true
                |   _ -> _helper norm (Seq.tail rst) in
        (_helper id coll, _helper not coll)

    let existsI pred coll = fst <| existsForallI pred coll
    let existsR pred coll = fst <| existsForallR pred coll

    let forallI pred coll = snd <| existsForallI pred coll
    let forallR pred coll = snd <| existsForallR pred coll

    let mapI proc coll = 
        let rec iter rst acc =
            match Seq.isEmpty rst with
            |   true -> acc
            |   _ -> iter (Seq.tail rst) <|
                    (Seq.append [(proc (Seq.head rst))] acc) in
        iter (Seq.rev coll) Seq.empty

    let rec mapR proc coll = 
        match Seq.isEmpty coll with
        |   true -> Seq.empty
        |   _ -> Seq.append [(proc (Seq.head coll))] <|
                (mapR proc (Seq.tail coll))

    let iterI proc coll = 
        let rec iter rst =
            match Seq.isEmpty rst with
            |   true -> ()
            |   _ -> proc (Seq.head rst) ; iter (Seq.tail rst) in
        iter coll

    let rec iterR proc coll = 
        match Seq.isEmpty coll with
        |   true -> ()
        |   _ ->
            proc (Seq.head coll) ; iterR proc (Seq.tail coll)
    
    let partitionI pred coll = 
        let rec iter rst acc =
            match Seq.isEmpty rst with
            |   true -> acc
            |   _ ->
                let (x, xs) = (Seq.head rst, Seq.tail rst) in
                match pred x with
                |   true -> iter xs (Seq.append [x] (fst acc), snd acc)
                |   _ -> iter xs (fst acc, Seq.append [x] (snd acc)) in
        iter (Seq.rev coll) (Seq.empty, Seq.empty)

    let partitionR pred coll = 
        let rec _helper norm rst =
            match Seq.isEmpty rst with
            |   true -> Seq.empty
            |   _ ->
                let (x, xs) = (Seq.head rst, Seq.tail rst) in
                match norm (pred x) with
                |   true -> Seq.append [x] (_helper norm xs)
                |   _ -> _helper norm xs in
        (_helper id coll, _helper not coll)

    let filterI pred coll = fst <| partitionI pred coll
    let filterR pred coll = fst <| partitionR pred coll

    let removeI pred coll = snd <| partitionI pred coll
    let removeR pred coll = snd <| partitionR pred coll

    let foldLeftI corp init coll = 
        let rec iter acc rst =
            match Seq.isEmpty rst with
            |   true -> acc
            |   _ -> iter (corp acc (Seq.head rst)) (Seq.tail rst) in
        iter init coll

    let rec foldLeftR corp init coll =
        match Seq.isEmpty coll with
        |   true -> init
        |   _ -> foldLeftR corp (corp init (Seq.head coll)) <| Seq.tail coll

    let foldRightI proc coll init = 
        let rec iter rst acc =
            match Seq.isEmpty rst with
            |   true -> acc
            |   _ -> iter (Seq.tail rst) (proc (Seq.head rst) acc) in
        iter (Seq.rev coll) init

    let rec foldRightR proc coll init =
        match Seq.isEmpty coll with
        |   true -> init
        |   _ -> proc (Seq.head coll) (foldRightR proc (Seq.tail coll) init)

    let unfoldRightI func seed =
        let rec iter cur acc =
            match func cur with
            | None -> acc
            | Some (a, newCur) -> iter newCur (Seq.append [a] acc) in
        iter seed Seq.empty

    let rec unfoldLeftR func seed =
        match func seed with
        | None -> Seq.empty
        | Some (a, newSeed) -> Seq.append [a] (unfoldLeftR func newSeed)

    let isOrderedI cmpfn keyfn coll =
        match Seq.length coll with
        | 0 -> true
        | _ ->
            let (x, xs) = (Seq.head coll, Seq.tail coll) in
            let rec iter rst oldval acc =
                match Seq.isEmpty rst with
                | true -> acc
                | _ -> 
                    let (y, ys) = (Seq.head rst, Seq.tail rst) in
                    iter ys y ((cmpfn (keyfn oldval) (keyfn y)) && acc) in
            iter xs x true

    let rec isOrderedR cmpfn keyfn coll =
        match Seq.length coll with
        | 0 -> true
        | 1 -> true
        | _ -> 
            let (y, z, zs) = (Seq.head coll, Seq.head (Seq.tail coll),
                    Seq.tail <| Seq.tail coll) in
            (cmpfn (keyfn y) (keyfn z)) && 
                (isOrderedR cmpfn keyfn (Seq.append [z] zs))
    
    
    let appendI xss yss =
        let rec iter rst acc = 
            match Seq.isEmpty rst with
            |   true -> acc
            |   _ -> iter (Seq.tail rst) (Seq.append [Seq.head rst] acc) in
        iter (Seq.rev xss) yss
    
    let rec appendR xss yss =
        match Seq.isEmpty xss with
        |   true -> yss
        |   _ -> Seq.append [Seq.head xss] (appendR (Seq.tail xss) yss)
    
    let interleaveI xss yss =
        let rec iter rst1 rst2 acc = 
            match (Seq.isEmpty rst1, Seq.isEmpty rst2) with
            |   (true, _) -> Seq.append (Seq.rev acc) rst2
            |   (_, true) -> Seq.append (Seq.rev acc) rst1
            |   _ -> 
                let (x, xs) = (Seq.head rst1, Seq.tail rst1) in
                let (y, ys) = (Seq.head rst2, Seq.tail rst2) in
                iter xs ys (Seq.append [y; x] acc) in
        iter xss yss Seq.empty
    
    let rec interleaveR xss yss =
        match (Seq.isEmpty xss, Seq.isEmpty yss) with
        |   (true, _) -> yss
        |   (_, true) -> xss
        |   _ ->
            Seq.append [Seq.head xss] (interleaveR yss (Seq.tail xss))

    let map2I proc xss yss =
        let rec iter wss zss acc =
            match (Seq.isEmpty wss, Seq.isEmpty zss) with
            |   (true, _) -> acc
            |   (_, true) -> acc
            |   _ -> 
                let (x, xs) = (Seq.head wss, Seq.tail wss) in
                let (y, ys) = (Seq.head zss, Seq.tail zss) in
                iter xs ys (Seq.append acc [proc x y]) in
        iter xss yss Seq.empty

    let rec map2R proc xss yss =
        match (Seq.isEmpty xss, Seq.isEmpty yss) with
        |   (true, _) -> Seq.empty
        |   (_, true) -> Seq.empty
        |   _ -> 
            let (x, xs) = (Seq.head xss, Seq.tail xss) in
            let (y, ys) = (Seq.head yss, Seq.tail yss) in
            Seq.append [proc x y] (map2R proc xs ys)
    
    let zipI xss yss = map2I (fun a b -> (a, b)) xss yss
    let zipR xss yss = map2R (fun a b -> (a, b)) xss yss
    
    let unzip2I xss =
        let rec iter rst acc =
            match Seq.isEmpty rst with
            |   true -> acc
            |   _ ->
                let (x, xs) = (Seq.head rst, Seq.tail rst) in
                iter xs (Seq.append [fst x] (fst acc),
                    Seq.append [snd x] (snd acc)) in
        iter (Seq.rev xss) (Seq.empty, Seq.empty)

    let concatI nseqs =
        match Seq.isEmpty nseqs with
        |   true -> Seq.empty
        |   _ ->
            let rec iter acc rst =
                match Seq.isEmpty rst with
                |   true -> Seq.rev acc
                |   _ -> iter (Seq.append (Seq.rev (Seq.head rst)) acc) <|
                        (Seq.tail rst) in 
            iter (Seq.rev (Seq.head nseqs)) (Seq.tail nseqs)

    let rec concatR nseqs =
        match Seq.isEmpty nseqs with
        |   true -> Seq.empty
        |   _ -> Seq.append (Seq.head nseqs) (concatR (Seq.tail nseqs))


    /// <summary>Lib main function.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    //[<EntryPoint>]
    let libmain (args: string[]) = 
        let (n, xs) = (3, [2; 1; 0; 4; 3]) in
        printfn "indexOf %d %s: %d" n (xs.ToString ())
            (Option.defaultValue -1 <| findIndexI (fun e -> n = e) xs)
        0

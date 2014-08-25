#light (*
exec fsharpi /nologo /warn:3 /checked /lib:build/bin/Debug,build/bin/Release `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs ini-parser` /r:Introfs.Practice.dll $0 $@
*)

namespace Introfs.Practice

//#r "Introfs.Util.dll" ;;
//#r "Introfs.Practice.dll" ;;

//open System

/// <summary>Sequenceops module.</summary>
module Sequenceops =
    
    let log = log4net.LogManager.GetLogger("prac")
    
    let findIndexSeqI (pred:'T -> bool) (lst:seq<'T>) =
        log.Info("findIndexSeqI()")
        let rec iter idx rst =
            match Seq.length rst with
            |   0 -> -1
            |   _ -> 
                match pred (Seq.head rst) with
                |   true -> idx
                |   _ -> iter (idx + 1) (Seq.tail rst)
        in iter 0 lst
    
    let rec revSeqR (lst:seq<'T>) =
        match Seq.length lst with
        |   0 -> Seq.empty
        |   _ -> Seq.append (revSeqR <| Seq.tail lst) [|Seq.head lst|]
    
    let revSeqI (lst:seq<'T>) =
        let rec iter rst acc =
            match Seq.length rst with
            |   0 -> acc
            |   _ -> iter (Seq.tail rst) (Seq.append [|Seq.head rst|] acc)
        in iter lst Seq.empty

    /// <summary>Lib main function.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    //[<EntryPoint>]
    let libmain (args: string[]) = 
        let (n, xs) = (3, [2; 1; 0; 4; 3]) in
        printfn "indexOf %d %s: %d" n (xs.ToString ()) <|
            findIndexSeqI (fun e -> n = e) xs
        0

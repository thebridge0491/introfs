#light (*
exec fsharpi /nologo /warn:3 /checked /lib:build/bin/Debug,build/bin/Release `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs ini-parser` /r:Introfs.Intro.dll $0 $@
*)

namespace Introfs.Intro

//#r "Introfs.Util.dll" ;;
//#r "Introfs.Intro.dll" ;;

//open System

/// <summary>Library module.</summary>
module Library =
    
    module Util = Introfs.Util.Library
    
    let log = log4net.LogManager.GetLogger "root"
    
    /// <summary>Composes greeting.</summary>
    /// <param name="rsrcPath">A string</param>
    /// <param name="greetPath">A string</param>
    /// <param name="name">A string</param>
    /// <returns>The string greeting.</returns>
    let greeting (rsrcPath: string) (greetPath:string) (name:string) = 
        let buf = ref "" in
        log.Info "greeting()"
        
        try
            //buf := (new System.IO.StreamReader(rsrcPath + "/" + greetPath)).ReadToEnd().TrimEnd('\n')
            buf := System.IO.File.ReadAllText(rsrcPath + "/" + greetPath).TrimEnd('\n')
        with
        | exc0 -> 
            printfn "(exc: %s) Bad env var RSRC_PATH: %s\n" (exc0.ToString ()) rsrcPath
            try
                buf := (Util.getFromResources greetPath null).TrimEnd('\n')
            with
            | exc1 ->
                reraise ()
                (*Environment.Exit 1*)
        (*
        let istr = new System.IO.StreamReader (greetPath) in
        if -1 < (istr.Peek()) then
            buf := istr.ReadLine()
        *)
        (!buf) + name

    /// <summary>Waits for input character.</summary>
    /// <param name="delayFunc">A function</param>
    let rec delayChar delayFunc = 
        let (_) = (delayFunc ()) in
        printf "Type any character when ready."
        let input = System.Console.ReadLine () in
        let ch = 
            if "" = input then '\000'
            else System.Convert.ToChar(input.[0]) in
        try
            if '\000' = ch || '\n' = ch then delayChar delayFunc
            else ch
        with
            | exc -> 
                eprintfn "%s" exc.Message
                System.Environment.Exit 1 ; ch

    /// <summary>Lib main function.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    //[<EntryPoint>]
    let libmain (args: string[]) = 
        let funcDelay () = (fun () -> System.Threading.Thread.Sleep(3)) |> ignore in
        printfn "delayChar <lambda>"
        delayChar funcDelay
        0

#light (*
exec fsharpi /nologo /warn:3 /checked /lib:build/bin/Debug,build/bin/Release `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs ini-parser` /r:Introfs.Practice.dll $0 $@
*)

namespace Introfs.Practice

//#r "Introfs.Util.dll" ;;
//#r "Introfs.Practice.dll" ;;

//open System

/// <summary>Classic module.</summary>
module Classic =
    
    let log = log4net.LogManager.GetLogger("prac")
    
    let rec factR (n:int64) =
        match n with
        |   0L -> 1L
        |   _ -> n * (factR (n - 1L))
    
    let factI (n:int64) = 
        log.Info("factI()")
        let rec iter prod ct =
            match ct with
            |   0L -> prod
            |   _ -> iter (prod * ct) (ct - 1L)
        in iter 1L n
    
    let rec exptR (b:float32) (n:float32) =
        match n with
        |   0.0f -> 1.0f
        |   _ -> b * (exptR b (n - 1.0f))
    
    let exptI (b:float32) (n:float32) =
        let rec iter prod ct =
            match ct with
            |   0 -> prod
            |   _ -> iter (prod * b) (ct - 1)
        in iter 1.0f ((int)n)

    /// <summary>Lib main function.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    //[<EntryPoint>]
    let libmain (args: string[]) = 
        let n = 5L in
        printfn "fact %d: %d" n (factI n)
        0

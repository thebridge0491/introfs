#light (*
exec fsharpi /nologo /warn:3 /checked /lib:.,$HOME/nuget/packages `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs fsharp.core ini-parser-netstandard log4net system.diagnostics.textwritertracelistener mono.options newtonsoft.json yamldotnet` /r:Introfs.Intro.dll $0 $@
*)

namespace Introfs.Intro

//#r "Introfs.Util.dll" ;;
//#r "Introfs.Intro.dll" ;;

//#load "Person.fs" ;;

// single-line comment
(* multi-line comment
    -- run w/out compile --
    [sh | fsharpi [/r:Depn0.dll]] Script.fs
    > Namespace.Mod.Class.main [||] ;;

    -- run REPL, load script, & run --
    fsharpi
    > #r "Depn0.dll" ;;
    > #load "Script.fs" ;;
    > Namespace.Mod.Class.main [||] ;;
    
    -- help/info tools in REPL --
    #help, #quit, #load, #I, #r
    
    -- show type info
    ???
*)

open System
open System.Diagnostics
open System.Reflection
open Newtonsoft.Json

open Introfs.Practice
open Introfs.Intro

type OptsRecord =
    {mutable Name: string ; mutable Num : int ; mutable IsExpt2 : bool}

type ConstItems = ZERO = 0 | NUMZ = 26
type Kind = FLOAT | DOUBLE | DECIMAL | SH | UINT

// discriminated union
type UVar =
    |   Short of kind : Kind * value : int8
    |   ULong of kind : Kind * value : uint64
    |   Double of kind : Kind * value : float
    |   Float of kind : Kind * value : float32

type Defn =
    {Hostname: string; Domain: string; File1: PathExt; User1: NameAge}
and PathExt =
    {Path: string; Ext: string}
and NameAge =
    {Name: string; Age: int}

type PathExtS () =
    member val path = "" with get,set
    member val ext = "" with get,set

type NameAgeS () =
    member val name = "" with get,set
    member val age = 0 with get,set

type YamlConfig () =
    member val hostname = "" with get,set
    member val domain = "" with get,set
    member val file1 = PathExtS () with get,set
    member val user1 = NameAgeS () with get,set

/// <summary>App module.</summary>
module App =
    
    module Util = Introfs.Util.Library
    module Seqops = Sequenceops
    
    let defaultOptsRecord : OptsRecord = {Name = "John"; Num = 0; 
        IsExpt2 = false} in
    (*let defaultUser : User = {Name = "John"; Num = 0; 
        TimeIn = System.DateTime.Now} in*)
    let log = log4net.LogManager.GetLogger "root" in
    
    let assembly = Assembly.GetExecutingAssembly () in
    //let showHelp = false in
    
    //let progName = Environment.GetCommandLineArgs().[0] in
    let progName = IO.Path.GetFileName(Environment.GetCommandLineArgs().[0]) in
    
    let runIntro (rsrcPath: string) (optsRec: OptsRecord) =
        let (time1, stopwatch1) = (DateTime.Now, new Diagnostics.Stopwatch()) in
        let (_, greetPath) = (stopwatch1.Start (), "greet.txt") in
        let (ch, delayMSecs, seedp) = ('\000', 2500, time1.Millisecond) in
        let mutable uVar1 : UVar = Short(kind = Kind.SH, value = 100y) in
        (*let user1 = {defaultUser with Name = optsRec.Name; Num = optsRec.Num} in*)
        let mutable user1 = User(Name = optsRec.Name, Num = optsRec.Num) in
        let (pers1, lst) = (Person("I.M. Computer", 32), [2; 1; 0; 4; 3]) in
        let strLst = Util.mkString (fun e -> sprintf "%d" e) lst in
        let rnd = new Random(seedp) in
        let (numVal, numArr) = (ref 0, [|0b1011; 0o13; 0xb; 11|]) in
        let tz1 = TimeZone.CurrentTimeZone in
        let tzStr = 
            if tz1.IsDaylightSavingTime(time1) then tz1.DaylightName
            else tz1.StandardName in
        let answer = ClassicPuzzles.nqueens 8 in
        let queensNdx = rnd.Next(0, List.length answer) in
        let rec pat = @"(?i)quit" 
            and m = Text.RegularExpressions.Regex.Match(optsRec.Name, pat) in
        
        numVal := Array.fold (+) 0 numArr
        assert ((!numVal) = numArr.Length * numArr.[0])
        
        Library.delayChar (fun () -> Threading.Thread.Sleep(delayMSecs)) |> ignore
        
        uVar1 <- ULong(kind = Kind.UINT, value = 1UL)
        uVar1 <- Float(kind = Kind.FLOAT, value = 1000.0f)
        uVar1 <- Double(kind = Kind.DOUBLE, value = 100.0)
        assert (match uVar1 with
                | Double (kind, value) -> Kind.DOUBLE = kind && 100.0 = value
                | _ -> false)
        
        user1.Num <- (if 0 = user1.Num then rnd.Next(0, 18) + 2 else user1.Num)
                
        printfn "%s match: %s to %s" (if m.Success then "Good" else "Does not")
            pat optsRec.Name
        
        printfn "%s %s\n%s!" (time1.ToString("ddd MMM dd HH:mm:ss yyyy zzz"))
            tzStr <| Library.greeting rsrcPath greetPath (user1.Name)
        stopwatch1.Stop ()
        printfn "(program %s) Took %.2f seconds." progName (float 
            (stopwatch1.ElapsedMilliseconds) * 1.0e-3)
        
        if optsRec.IsExpt2 then
            printfn "expt 2.0 %.1f: %.1f" (float32 user1.Num) <|
                Classic.exptI 2.0f (float32 user1.Num)
            printfn "rev %s: %s" strLst 
                (Util.mkString (fun e -> sprintf "%d" e) <| Seqops.revI lst)
            printfn "List.sortWith <lambda> %s: %s" strLst 
                (Util.mkString (fun e -> sprintf "%d" e) <|
                    List.sortWith (fun a b -> compare b a) lst)
        else
            printfn "fact %d: %d" user1.Num <| Classic.factI (int64 user1.Num)
            printfn "findIndex <lambda> %s: %A" strLst <|
                Option.get (Seqops.findIndexI (fun e -> e = 3) lst)
            printfn "List.append %s %s: %s" strLst
                (Util.mkString (fun e -> sprintf "%d" e) [9; 9; 9; 9;])
                (Util.mkString (fun e -> sprintf "%d" e) <| lst @ [9; 9; 9; 9;])
        
        printfn "%s" (String.replicate 40 "#")
        printfn "pascaltri %d: %s\n" 5 (Util.mkString
            (fun xs -> Util.mkString string xs) (Classic.pascaltriAdd 5))
        printfn "%s" <| Util.mkStringNested ("", "", "")
            (fun xs -> Util.mkStringInit (" ", " ", "\n") string xs)
            (Classic.pascaltriAdd 5)
        
        printfn "%s" (String.replicate 40 "#")
        printfn "hanoiMoves (%d, %d %d) %d: %s\n" 1 2 3 4
            (Util.mkString (fun (p1, p2) -> Printf.sprintf "(%d, %d)" p1 p2)
            (match ClassicPuzzles.hanoiMoves (1, 2, 3) 4 with
            | res, _, _ -> res))
        printfn "%s" <| Util.mkStringInit ("", "\n", "\n")
            (fun (moves, pegs) -> Printf.sprintf "(%s, %s)" moves (
                Util.mkString (Util.mkString string) pegs))
                (match ClassicPuzzles.hanoiMoves (1, 2, 3) 4 with
                | _, _, mov -> mov)
        
        printfn "%s" (String.replicate 40 "#")
        printfn "nqueensGrid %d answer: %s\n" 8 (Util.mkString (fun (h, t) ->
            Printf.sprintf "(%d, %d)" h t) (List.item queensNdx answer))
        printfn "%s" <| Util.mkStringNested ("", "", "") (fun xs -> 
            Util.mkStringInit (" ", "-", "-\n") string xs)
            (ClassicPuzzles.nqueensGrid 8 (List.item queensNdx answer))
        
        printfn "%s" (String.replicate 40 "#")
        assert (pers1.GetType().Equals typeof<Person>)
        assert (pers1.Equals pers1)
        printfn "person1.ToString: %s" (pers1.ToString ())
        printfn "person1.Age: %d" pers1.Age
        pers1.Age <- 33
        printfn "person1.Age <- %d" 33
        printfn "person1.ToString: %s" (pers1.ToString ())
        printfn "%s" (String.replicate 40 "#")
    
    let parseCmdopts (args: string[]) (options: Mono.Options.OptionSet) = 
        let extra = ref (new Collections.Generic.List<string>()) in
        log.Info "parseCmdopts()"
        try
            extra := options.Parse args
        with
        | :? Mono.Options.OptionException as exc -> 
            eprintfn "%s: %s" progName (exc.Message)
            Environment.Exit(1)
        if 0 < (!extra).Count then eprintfn "Extra args: %d" ((!extra).Count)
    
    /// <summary>Main entry point.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    [<EntryPoint>]
    let main (args: string[]) = 
        let optsRec = {defaultOptsRecord with Name = "World"} in
        let options = new Mono.Options.OptionSet() in
        ["u|user=", "user name", (fun v -> optsRec.Name <- string v)
            ; "n|num=", "number", (fun v -> optsRec.Num <- int v)
            ; "2|expt2", "expt 2 n", (fun v -> optsRec.IsExpt2 <- true)
            ; "h|help",  "show this message", (fun v -> 
                eprintfn "Usage: %s [options]\n\nOptions:" progName
                options.WriteOptionDescriptions Console.Out
                Environment.Exit 1)
            ] |> List.iter (fun (o, d, f) -> options.Add(o, d, f) |> ignore)
        
        let traceOut = IO.File.Create "trace.log" in
        let lstnrConsole = new TextWriterTraceListener(System.Console.Out) in
        let lstnrText = new TextWriterTraceListener(traceOut) in
        // /define:[TRACE|DEBUG]
        Trace.Listeners.Add(lstnrConsole) |> ignore
        Trace.Listeners.Add(lstnrText) |> ignore
        
        parseCmdopts args options
        
        let envRsrcPath = Environment.GetEnvironmentVariable("RSRC_PATH") in
        let rsrcPath = 
            match isNull envRsrcPath with
            | true -> "resources" | _ -> envRsrcPath in
        
        let (iniStr, jsonStr, yamlStr) = (ref @"", ref @"", ref @"") in
        try
            //iniStr := (new IO.StreamReader(rsrcPath + "/prac.conf")).ReadToEnd()
            iniStr := IO.File.ReadAllText(rsrcPath + "/prac.conf")
            jsonStr := IO.File.ReadAllText(rsrcPath + "/prac.json")
            yamlStr := IO.File.ReadAllText(rsrcPath + "/prac.yaml")
        with
        | exc0 -> 
            printfn "(exc: %s) Bad env var RSRC_PATH: %s\n" (exc0.ToString ()) rsrcPath
            try
                iniStr := Util.getFromResources "prac.conf" assembly null
                jsonStr := Util.getFromResources "prac.json" assembly <|
                    (assembly.GetName ()).Name + ".resources"
                yamlStr := Util.getFromResources "prac.yaml" assembly null
            with
            | exc1 ->
                reraise ()
                Environment.Exit 1
        
        //let cfgIni = new KeyFile.GKeyFile() in
        //cfgIni.LoadFromData (!iniStr)
        let cfgIni = (new IniParser.Parser.IniDataParser()).Parse(!iniStr) in
        (*
        let defaultDefn : Defn = {
            Hostname = ""; Domain = ""; File1 = {Path = ""; Ext = ""}; 
            User1 = {Name = ""; Age = 0} } in
        let defn1 = defaultDefn in
        let anonType = Newtonsoft.Json.JsonConvert.DeserializeAnonymousType (!jsonStr, defn1) in
        let mapRootJson = Newtonsoft.Json.JsonConvert.DeserializeObject<
            Map<string, obj>> (!jsonStr) in
        let mapUserJson = Newtonsoft.Json.JsonConvert.DeserializeObject<
            Map<string, obj>> (mapRootJson.["user1"].ToString ()) in
        
        let deserializer = new YamlDotNet.Serialization.Deserializer () in
        let dictRootYaml = deserializer.Deserialize<YamlConfig>(
            new IO.StringReader (!yamlStr)) in
        *)
        let arrTups = [|
            //(Util.iniCfgToStr cfgIni
            //    , cfgIni.GetValue("default", "domain")
            //    , cfgIni.GetValue("user1", "name"))
            (Util.iniCfgToStr cfgIni
                , cfgIni.["default"].["domain"]
                , cfgIni.["user1"].["name"])
            (* ; (anonType.ToString ()
                , anonType.Domain
                , anonType.User1.Name)
            ; (Util.mkStringInit ("map [", "; ", "]") string <| 
                    Map.toSeq mapRootJson
                , mapRootJson.["domain"].ToString ()
                , mapUserJson.["name"].ToString ())
            ; (!yamlStr
                , dictRootYaml.domain
                , dictRootYaml.user1.name) *)
            |] in
        Array.iter (fun (t0, t1, t2) -> 
            printfn "config: %s" t0
            printfn "domain: %s" t1
            printfn "user1Name: %s\n" t2) arrTups
        
        runIntro rsrcPath optsRec
        
        //Trace.Fail "Trace example"
        Trace.Flush ()
        traceOut.Close ()
        lstnrConsole.Close ()
        lstnrText.Close ()
        0

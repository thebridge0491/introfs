#light (*
exec fsharpi /nologo /warn:3 /checked /lib:build/bin/Debug,build/bin/Release `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs ini-parser Mono.Options log4net Newtonsoft.Json YamlDotNet` /r:Introfs.Intro.dll $0 $@
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

type OptsRecord =
    {mutable Name: string}

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
    
    let defaultOptsRecord : OptsRecord = {Name = "John"} in
    
    let log = log4net.LogManager.GetLogger "root" in
    
    let assembly = Assembly.GetExecutingAssembly () in
    //let showHelp = false in
    
    //let progName = Environment.GetCommandLineArgs().[0] in
    let progName = IO.Path.GetFileName(Environment.GetCommandLineArgs().[0]) in
    
    let runIntro (name: string) =
        let rec pat = @"(?i)quit" 
            and m = Text.RegularExpressions.Regex.Match(name, pat) in
        printfn "%s match: %s to %s" (if m.Success then "Good" else "Does not")
            pat name
    
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
            ; "h|help",  "show this message", (fun v -> 
                eprintfn "Usage: %s [options]\n\nOptions:" progName
                options.WriteOptionDescriptions Console.Out
                Environment.Exit 1)
            ] |> List.iter (fun (o, d, f) -> options.Add(o, d, f) |> ignore)
        
        let traceOut = IO.File.Create "trace.log" in
        let lstnrConsole = new ConsoleTraceListener(true) in
        let lstnrText = new TextWriterTraceListener(traceOut) in
        // /define:[TRACE|DEBUG]
        Debug.Listeners.Add(lstnrConsole) |> ignore
        Debug.Listeners.Add(lstnrText) |> ignore
        
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
                iniStr := Library.getFromResources "prac.conf" null
                jsonStr := Library.getFromResources "prac.json" <|
                    (assembly.GetType ()).Namespace + ".resources"
                yamlStr := Library.getFromResources "prac.yaml" null
            with
            | exc1 ->
                reraise ()
                Environment.Exit 1
        
        //let cfgIni = new KeyFile.GKeyFile() in
        //cfgIni.LoadFromData (!iniStr)
        let cfgIni = (new IniParser.StringIniParser()).ParseString(!iniStr) in
        
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
        
        let arrTups = [|
            //(Library.iniCfgToStr cfgIni
            //    , cfgIni.GetValue("default", "domain")
            //    , cfgIni.GetValue("user1", "name"))
            (Library.iniCfgToStr cfgIni
                , cfgIni.["default"].["domain"]
                , cfgIni.["user1"].["name"])
            ; (anonType.ToString ()
                , anonType.Domain
                , anonType.User1.Name)
            ; (Library.mkStringInit ("map [", "; ", "]") string <| 
                    Map.toSeq mapRootJson
                , mapRootJson.["domain"].ToString ()
                , mapUserJson.["name"].ToString ())
            ; (!yamlStr
                , dictRootYaml.domain
                , dictRootYaml.user1.name)
            |] in
        Array.iter (fun (t0, t1, t2) -> 
            printfn "config: %s" t0
            printfn "domain: %s" t1
            printfn "user1Name: %s\n" t2) arrTups
        
        runIntro optsRec.Name
        
        //Trace.Fail "Trace example"
        Trace.Flush () //Debug.Flush ()
        traceOut.Close ()
        lstnrConsole.Close ()
        lstnrText.Close ()
        0

#light (*
exec fsharpi /nologo /warn:3 /checked /lib:build/bin/Debug,build/bin/Release `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs ini-parser` /r:Introfs.Util.dll $0 $@
*)

namespace Introfs.Util

//#r "Introfs.Util.dll" ;;
//#r "Introfs.Util.dll" ;;

//open System
open System.Reflection

/// <summary>Library module.</summary>
module Library =

    /// <summary>Creates string representation of sequence, using strings
    /// (beg, sep, stop).</summary>
    /// <param name="beg">A string</param>
    /// <param name="sep">A string</param>
    /// <param name="stop">A string</param>
    /// <param name="fmt">A function to format elements.</param>
    /// <param name="coll">A sequence.</param>
    /// <returns>The string representation.</returns>
    let mkStringInit (beg: string, sep, stop) fmt (coll: seq<'T>) =
        match Seq.length coll with
        |   0 -> beg + stop
        |   _ -> beg + (Seq.foldBack (fun el acc -> (fmt el) +
            (if "" = acc then "" else sep) + acc) coll "") + stop

    /// <summary>Creates default string representation of sequence.</summary>
    /// <param name="fmt">A function to format elements.</param>
    /// <param name="coll">A sequence.</param>
    /// <returns>The string representation.</returns>
    let mkString fmt (coll: seq<'T>) = mkStringInit ("[", "; ", "]") fmt coll

    /// <summary>Creates string representation of nested sequence.</summary>
    /// <param name="beg">A string</param>
    /// <param name="sep">A string</param>
    /// <param name="stop">A string</param>
    /// <param name="fmt">A function to format collection.</param>
    /// <param name="ncoll">A nested sequence.</param>
    /// <returns>The string representation.</returns>
    let mkStringNested (beg: string, sep, stop) fmt (ncoll: seq<'T>) =
        match Seq.length ncoll with
        |   0 -> beg + stop
        |   _ -> (Seq.fold (fun acc coll -> acc + (fmt coll) + sep) beg
            ncoll) + stop

    /// <summary>Get file contents from embedded resources.</summary>
    /// <param name="rsrcFileNm">A string.</param>
    /// <param name="assy">An assembly or null.</param>
    /// <param name="prefix">A string or null.</param>
    /// <returns>The file contents.</returns>
    let getFromResources (rsrcFileNm: string) (assy: Assembly) (prefix: string) =
        let assembly = if (not <| isNull assy) then assy else Assembly.GetExecutingAssembly () in
        let pathPfx = if (not <| isNull prefix) then prefix else
            (assembly.GetName ()).Name + ".resources" in
        using (if not <| isNull (assembly.GetManifestResourceStream rsrcFileNm) then (assembly.GetManifestResourceStream rsrcFileNm) else (assembly.GetManifestResourceStream <| pathPfx + "." +  rsrcFileNm)) (fun strm ->
            using (new System.IO.StreamReader(strm)) (fun reader ->
                reader.ReadToEnd ())
            )

    /// <summary>Create string representation of ini config.</summary>
    /// <param name="cfg">An Ini config.</param>
    /// <returns>The string representation.</returns>
    let iniCfgToStr (cfg: IniParser.Model.IniData) =
        let mapIni = ref Map.empty in
        match isNull cfg with
        | true -> mkStringInit ("map [", "; ", "]") string <| Map.toSeq !mapIni
        | _ ->
            (* // cfg: KeyFile.GKeyFile
            for grp in (cfg.GetGroups ()) do
                for key in (cfg.GetKeys grp) do
                    mapIni := Map.add (grp + ":" + key)
                        cfg.GetValue(grp, key) !mapIni*)
            // cfg: IniParser.IniData
            for sect in cfg.Sections do
                for key in cfg.[sect.SectionName] do
                    mapIni := Map.add (sect.SectionName + ":" + key.KeyName)
                        cfg.[sect.SectionName].[key.KeyName] !mapIni
            mkStringInit ("map [", "; ", "]") string <| Map.toSeq !mapIni

    /// <summary>Compares equality within tolerance.</summary>
    /// <param name="tolerance">A double</param>
    /// <param name="a">A double</param>
    /// <param name="b">A double</param>
    /// <returns>The truth result of comparison.</returns>
    let inEpsilon (tolerance: float) (a: float) (b: float) =
        let delta = abs tolerance in
        (* (a - delta) <= b && (a + delta) >= b) *)
        not ((a + delta) < b) && not ((b + delta) < a)

    /// <summary>Creates Cartesian product of two sequences.</summary>
    /// <param name="xs">A sequence</param>
    /// <param name="ys">A sequence</param>
    /// <returns>The cartesian product.</returns>
    let cartesianProd (xs: seq<'T>) (ys: seq<'T>) =
        (*[for x in xs do for y in ys do if true -> (x, y)]*)
        (*[for x in xs do for y in ys do if true then yield (x, y)]*)
        Seq.concat <| Seq.map (fun x ->
            Seq.filter (fun e -> true) <|
                Seq.map (fun y -> (x, y)) ys) xs

    /// <summary>Lib main function.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    //[<EntryPoint>]
    let libmain (args: string[]) =
        let (xs, ys) = ([0; 1; 2], [10; 20; 30]) in
        printfn "cartesianProd %s %s: %s" (mkString string xs)
            (mkString string ys) (mkString string (cartesianProd xs ys))
        0

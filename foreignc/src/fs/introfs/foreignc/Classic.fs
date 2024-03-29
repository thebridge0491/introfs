#light (*
exec fsharpi /nologo /warn:3 /checked /lib:.,$HOME/nuget/packages `pkg-config --with-path=$HOME/.local/lib/pkgconfig --libs fsharp.core` /r:Introfs.Foreignc.dll $0 $@
*)

namespace Introfs.Foreignc

//#r "Introfs.Foreignc.dll" ;;
//#r "Introfs.Foreignc.dll" ;;

//open System
open System.Runtime.InteropServices
//open Microsoft.FSharp.NativeInterop

/// <summary>Classic module.</summary>
module Classic =
    
    [<System.Security.SuppressUnmanagedCodeSecurityAttribute>]
    module private SafeNativeMethods =
        [<DllImport("libintro_c-practice.so", EntryPoint="fact_i")>]
        extern int64 fact_i(int n)

        [<DllImport("libintro_c-practice.so")>]
        extern int64 fact_lp(int n)

        [<DllImport("libintro_c-practice.so")>]
        extern float32 expt_i(float32 b, float32 n)

        [<DllImport("libintro_c-practice.so")>]
        extern float32 expt_lp(float32 b, float32 n)

    /// <summary>Computes factorial (loop version).</summary>
    /// <param name="n">A long</param>
    /// <returns>The factorial of number.</returns>
    let factLp (n: int64) = SafeNativeMethods.fact_lp(int n)

    /// <summary>Computes factorial (iterative version).</summary>
    /// <param name="n">A long</param>
    /// <returns>The factorial of number.</returns>
    let factI (n: int64) = SafeNativeMethods.fact_i(int n)
    
    /// <summary>Computes n-th exponent of base.(loop version).</summary>
    /// <param name="b">A float</param>
    /// <param name="n">A float</param>
    /// <returns>The n-th exponent of base.</returns>
    let exptLp (b: float32) (n: float32) = SafeNativeMethods.expt_lp(b, n)

    /// <summary>Computes n-th exponent of base.(iterative version).</summary>
    /// <param name="b">A float</param>
    /// <param name="n">A float</param>
    /// <returns>The n-th exponent of base.</returns>
    let exptI (b: float32) (n: float32) = SafeNativeMethods.expt_i(b, n)
    
    /// <summary>Lib main function.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    //[<EntryPoint>]
    let libmain (args: string[]) = 
        let n = 5L in
        printfn "fact %d: %d" n <| factI n
        0

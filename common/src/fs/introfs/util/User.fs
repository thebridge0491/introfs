namespace Introfs.Util


//open System

/// <summary>User type.</summary>
/// <typeparam name="Name">A string</typeparam>
/// <typeparam name="Num">An integer</typeparam>
/// <typeparam name="TimeIn">A DateTime value</typeparam>
type User = 
    {mutable Name: string; mutable Num: int; mutable TimeIn: System.DateTime}
    with
    override this.ToString () = 
        sprintf "%s: {Name: %s; Num: %d; TimeIn: %A}" (this.GetType().Name) 
            (this.Name) (this.Num) (this.TimeIn)    

namespace Introfs.Intro

/// <summary>Person class.</summary>
[<AllowNullLiteral>]
type Person(name0: string, age0: int) = 
    let mutable name = name0
    let mutable age = age0
    
    let log = log4net.LogManager.GetLogger "root"
    
    do
        log.Debug "Person()"
    
    /// <value>Gets|Sets the person's name.</value>
    member this.Name 
        with get() = name
        and set(value) = name <- value
    
    /// <value>Gets|Sets the person's age.</value>
    member this.Age 
        with get() = age
        and set(value) = age <- value
    (*
    interface System.IEquatable<Person> with
        member this.Equals (other: Person) =
            match isNull other with
            | true -> false
            | _ -> (name, age) = (other.Name, other.Age)
    
    /// <inheritdoc/>
    override this.Equals other =
        match other with
        | :? Person as p -> this.Equals p
        | _ -> false
    *)
    /// <inheritdoc/>
    override this.Equals other =
        match other with
        | :? Person as p -> 
            match isNull p with
            | true -> false
            | _ -> (name, age) = (p.Name, p.Age)
        | _ -> false
    
    /// <inheritdoc/>
    override this.ToString () = 
        sprintf "%s: {name: %s; age: %d}" (this.GetType().Name) (this.Name) 
            (this.Age)
 
    /// <inheritdoc/>
    override this.GetHashCode () =
        hash(this.Name, this.Age)
        //this.ToString().GetHashCode()

open System

for i in [0 .. 7] do
    printfn "(%d): %A" i <| Guid.NewGuid ()

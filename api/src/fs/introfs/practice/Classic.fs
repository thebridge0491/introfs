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

    let exptI b n = 
        let rec iter prod ct = 
            match ct with
            |   0 -> prod
            |   _ -> iter (prod * b) (ct - 1) in
        iter 1.0f <| int n

    let rec exptR b n = 
        match n with
        |   0.0f -> 1.0f
        |   _ -> b * (exptR b (n - 1.0f))
    
    let squareI n = exptI n 2.0f  //n * n //int (n ** 2.0f)
    let squareR n = exptR n 2.0f
    
    let fastExptI (b:float32) (n:float32) =
        let rec iter prod ct =
            match (ct = 0, 0 = (ct % 2)) with
            |   (true, _) -> prod
            |   (_, true) -> iter (prod * (b ** 2.0f)) (ct - 2)
            |   (_, _) -> iter (prod * b) (ct - 1) in
        iter 1.0f <| int n
    
    let rec fastExptR (b:float32) (n:float32) =
        match (n = 0.0f, 0.0f = (n % 2.0f)) with
        |   (true, _) -> 1.0f
        |   (_, true) -> (fastExptR b (n / 2.0f)) ** 2.0f
        |   (_, _) -> b * (fastExptR b (n - 1.0f))
    
    let numseqMathI init op hi lo =
        let rec iter start acc =
            match start < lo with
            |   true -> acc
            |   _ -> iter (start - 1L) (op acc start) in
        iter hi init
    
    let rec numseqMathR init op hi lo =
        match hi < lo with
        |   true -> init
        |   _ -> op hi (numseqMathR init op (hi - 1L) lo)
    
    let sumToI hi lo = numseqMathI 0L (+) hi lo
    let sumToR hi lo = numseqMathR 0L (+) hi lo
    
    let factI n = log.Info "factI()" ; numseqMathI 1L ( * ) n 1L
    let factR n = numseqMathR 1L ( * ) n 1L
    
    let fibI n =
        let rec iter sum1 sum0 ct =
            match ct with
            |   0 -> sum0
            |   _ -> iter (sum1 + sum0) sum1 (ct - 1) in
        iter 1 0 n
    
    let rec fibR n =
        match (n = 0 || n = 1) with
        |   true -> n
        |   _ -> (fibR (n - 1)) + (fibR (n - 2))
    
    let pascaltriAdd rows =
        let nextRow xs = List.map2 (fun a b -> a + b) (0::xs) (xs @ [0]) in
        let rec triangle xs rows =
            match rows with
            |   0 -> []
            |   _ -> xs :: (triangle (nextRow xs) (rows - 1)) in
        triangle [1] (rows + 1)
    
    let pascaltriMult rows =
        let pascalrow r =
            let rec iter col xs =
                match (r = col, xs) with
                |   (true, _) -> xs
                |   (_, []) -> raise (Failure "empty list")
                |   (_, y::_) ->
                    iter (col + 1) ((y * (r - col) / col) :: xs) in
            iter 1 [1] in
        List.map pascalrow [1 .. (rows + 1)]
    
    
    let quotRem (a:int) (b:int) =
        let q = (a / b) in
        (q, a - (q * b))
    
    let quotM (a:int) (b:int) =
        match (quotRem a b) with q, _ -> q
    let remM (a:int) (b:int) =
        match (quotRem a b) with _, r -> r
    
    let divMod (a:float32) (b:float32) =
        let q = (a / b) in
        (q, a - ((truncate q) * b))
    
    let divM (a:float32) (b:float32) =
        match (divMod a b) with d, _ -> d
    let modM (a:float32) (b:float32) =
        match (divMod a b) with _, m -> m
    
    
    let euclidI (m:int) (n:int) =
        let rec iter a b =
            match b with
            |   0 -> a
            |   _ -> iter b (a % b) in
        iter m n
    
    let rec euclidR (m:int) (n:int) =
        match n with
        |   0 -> m
        |   _ -> euclidR n (m % n)
    
    let gcdI (nss:int list) =
        match nss with
        |   [] -> 1
        |   x::xs ->
            let rec iter acc rst =
                match rst with
                |   [] -> acc
                |   m::ns -> iter (euclidI acc m) ns in
            iter x xs
    
    let rec gcdR (nss:int list) =
        match nss with
        |   [] -> 1
        |   m::[] -> m
        |   (m::n::rst) -> gcdR ((euclidR m n) :: rst)
    
    let lcmI (nss:int list) =
        match nss with
        |   [] -> 1
        |   x::xs ->
            let rec iter acc rst =
                match rst with
                |   [] -> acc
                |   m::ns -> iter ((acc * m) / (euclidI acc m)) ns in
            iter x xs
    
    let rec lcmR (nss:int list) =
        match nss with
        |   [] -> 1
        |   m::[] -> m
        |   (m::n::rst) -> lcmR (((m * n) / (euclidR m n)) :: rst)

    let baseExpandI b n =
        let rec iter q acc =
            match q with
            |   0 -> acc
            |   _ -> iter (q / b) ((q % b) :: acc) in
        iter n []

    let rec baseExpandR b n =
        match n with
        |   0 -> []
        |   _ -> (baseExpandR b (n / b)) @ [n % b]

    let baseTo10I b (nss:int list) =
        let rec iter xs acc ct =
            match xs with
            |   [] -> acc
            |   n::ns -> iter ns (acc + (n * (int ((float b) ** 
                    (float ct))))) (ct + 1) in
        iter (List.rev nss) 0 0

    let rec baseTo10R b (nss:int list) =
        match nss with
        |   [] -> 0
        |   (n::ns) -> (baseTo10R b ns) + (n * (int ((float b) ** 
                (float (List.length ns)))))

    let rangeStepI step start stop =
        let cmpOp = if step > 0 then (>) else (<) in
        let rec iter cur acc =
            match cmpOp cur stop with
            |   true -> acc
            |   _ -> iter (cur + step) (cur :: acc) in
        List.rev (iter start [])

    let rec rangeStepR step start stop =
        let cmpOp = (if step > 0 then (>) else (<)) in
        match cmpOp start stop with
            |   true -> []
            |   _ -> start :: (rangeStepR step (start + step) 
                    stop)

    let rangeI start stop = rangeStepI 1 start stop
    let rangeR start stop = rangeStepR 1 start stop

    let compose1 f g = (fun x -> f (g x))
    

    /// <summary>Lib main function.</summary>
    /// <param name="args">An array</param>
    /// <returns>The exit code.</returns>
    //[<EntryPoint>]
    let libmain (args: string[]) = 
        let n = 5L in
        printfn "fact %d: %d" n (factI n)
        0

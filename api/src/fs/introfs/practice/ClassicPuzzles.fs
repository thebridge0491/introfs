namespace Introfs.Practice

module ClassicPuzzles = 

    //open System

    let rec hanoi (src, dest, spare) numDisks =
        match numDisks <= 0 with
        |   true -> []
        |   _ ->
            (hanoi (src, spare, dest) (numDisks - 1)) @ [(src, dest)] @
                (hanoi (spare, dest, src) (numDisks - 1))

    let hanoiMoves (src, dest, spare) numDisks = 
    (*
        let hanoiPegs res =            (* mutable version using Array.set *)
            let rec iter vecPegs lst acc =
                match lst with
                |   [] -> acc
                |   x::xs ->
                    let (el1, el2) = ((fst x) - 1, (snd x) - 1) in
                    let lst2 = (Array.get vecPegs el2) in
                    let pegDnarr =
                        match (Array.get vecPegs el1) with
                        |   [] -> ()
                        |   d1::rst1 ->
                                Array.set vecPegs el1 rst1
                                ; Array.set vecPegs el2 (d1 :: lst2) in 
                        iter vecPegs xs ((Array.toList vecPegs) :: acc) in
            List.rev (iter [|[1 .. numDisks]; []; []|] res []) in
    *)
        let hanoiPegs res =            (* immutable version using dict/maps *)
            let map1 = List.fold (fun acc (idx, lst) -> 
                    Map.add idx lst acc) Map.empty
                        [(0, [1 .. numDisks]); (1, []); (2, [])] in 
            let rec iter mapPegs lst acc =
                match lst with
                |   [] -> acc
                |   x::xs -> 
                    let (el1, el2) = ((fst x) - 1, (snd x) - 1) in
                    let lst2 = (Map.find el2 mapPegs) in
                    let pegDnmap =
                        match (Map.find el1 mapPegs) with
                        |   [] -> Map.empty
                        |   d1::rst1 -> 
                            let pegUpmap = Map.add el1 rst1 mapPegs in
                            Map.add el2 (d1::lst2) pegUpmap in
                    iter pegDnmap xs ((List.fold (fun a i -> 
                        (Map.find i pegDnmap) :: a) [] [2; 1; 0]) :: acc) in
                List.rev (iter map1 res []) in 
    
        let statTxt resLen =
            let calcLen = (int (2.0 ** (float numDisks))) - 1 in
            Printf.sprintf "((n = %d) 2^n - 1 = %d %s (length(result) = %d\n"
                numDisks calcLen 
                (if calcLen = resLen then "==" else "<>") resLen in
        let txtFmt = Printf.sprintf "'move from %d to %d'" in
        let res = hanoi (src, dest, spare) numDisks in
        let proc = (fun (h, t) -> txtFmt h t) in
        (res, [statTxt (List.length res); String.replicate 40 "-"],
            List.zip (List.map proc res) (hanoiPegs res))

    let nqueens n =
        let threatp (x1, y1) (x2, y2) =
            (x1 = x2) || (y1 = y2) || ((abs (x1 - x2)) = (abs (y1 - y2))) in
        let rec safep (col, row) placedSet =
            match placedSet with
            |   [] -> true
            |   x::xs -> 
                    if threatp (col, row) x then false
                    else safep (col, row) xs in
        let rec iter col row placedSet board = 
            if (n - 1) < col then (List.rev placedSet :: board)
            else if (n - 1) < row then board
            else if safep (col, row) placedSet then
                iter col (row + 1) placedSet (iter (col + 1) 0
                    ((col, row) :: placedSet) board)
            else iter col (row + 1) placedSet board in
        iter 0 0 [] []
    (*
    let nqueensGrid numQueens answer =   (* mutable version using Array.set *)
        let lstN = [0 .. (numQueens - 1)] in
        let calcGrid el = abs ((el + 1) - numQueens) in
        let arr2d = Array.init (numQueens + 1) (fun a -> 
            Array.init (numQueens + 1) (fun e -> " ")) in
        List.iter (fun el -> Array.set arr2d.[calcGrid el] 0 (string el)) lstN
        ; List.iter (fun el -> Array.set arr2d.[numQueens] (el + 1) 
            (string (char (el + (int 'a')))) ) lstN
        ; List.iter (fun (h, t) -> Array.set arr2d.[calcGrid t] (h + 1) 
            "Q") answer
        ; arr2d
    *)
    let nqueensGrid numQueens answer =    (* immutable version *)
        let lstN = [0 .. (numQueens - 1)] in
        let mkRow acc (h:int, t:int) =
            let lstBlank = List.map (fun _ -> " ") lstN in 
            ((string t) :: (List.mapi (fun i e ->
                if i = h then "Q"
                else e) lstBlank)) :: acc in
        let lstLtrs = " " :: (List.map (fun x -> 
                string (char (x + (int 'a')))) lstN) in
        List.fold mkRow [lstLtrs] (List.sortWith (fun (_, at) (_, bt) -> 
            compare at bt) answer)

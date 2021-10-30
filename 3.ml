let at (n: int) (l: 'a list): 'a =
    let rec _at (toskip: int) (l: 'a list) =
        if toskip = 0 then
            match l with
            | [] -> failwith "za duze n"
            | hd :: tl -> hd
        else
            match l with
            | [] -> failwith "za duze n"
            | hd :: tl -> _at (toskip - 1) tl
    in
    _at n l

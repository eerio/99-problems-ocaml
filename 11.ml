type 'a rle =
    | One of 'a
    | Many of 'a * int

let encode (l: 'a list): 'a rle list =
    let _encode (acc, last, count) x =
        match last with
        | None ->
            ([], Some x, 1)
        | Some last ->
            if last = x then
                (acc, Some last, count + 1)
            else
                if count = 1 then
                    (One last :: acc, Some x, 1)
                else
                    (Many (last, count) :: acc, Some x, 1)

    in
    let acc, last, count = List.fold_left _encode ([], None, 0) l
    in
    match last with
        | None -> []
        | Some last ->
            if count = 0 then
                List.rev acc
            else
                List.rev ((Many (last, count)) :: acc)


type 'a code = 'a * int

let fst3 (a, b, c) = a

let encode (l: 'a list): 'a code list =
    let _encode (acc, last, count) x =
        match last with
        | None ->
            ([], Some x, 1)
        | Some last ->
            if last = x then
                (acc, Some last, count + 1)
            else
                ((last, count) :: acc, Some x, 1)
    in
    List.rev (fst3 (List.fold_left _encode ([], None, 0) l))

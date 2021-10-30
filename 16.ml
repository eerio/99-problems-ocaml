let drop (l: 'a list) (n: int): 'a list =
    let _drop (acc, i) x =
        if i mod n = 0 then
            (acc, 1)
        else
            (x::acc, i+1)
    in
    List.rev (fst (List.fold_left _drop ([], 1) l))

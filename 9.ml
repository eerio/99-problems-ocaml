let fst3 (a, b, c) = a

let pack (l: 'a list): 'a list list =
    let _pack (acc, temp_acc, last) x =
        match last with
        | None ->
            ([], [x], Some x)
        | Some last ->
            if x = last then
                (acc, x :: temp_acc, Some last)
            else
                (temp_acc :: acc, [x], Some x)
    in
    List.rev (fst3 (List.fold_left _pack ([], [], None) l))

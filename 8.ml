let compress (l: 'a list) =
    let _compress (acc, last) x =
        match last with
        | Some (last) ->
            if x = last then
                (acc, Some last)
            else
                (x :: acc, Some x)
        | None ->
            (x :: acc, Some x)
    in
    List.rev (fst (List.fold_left _compress ([], None) l))

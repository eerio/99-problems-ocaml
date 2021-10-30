
let extract (n: int) (lst: 'a list): ('a * 'a) list =
    let addsnd acc first tail =
        List.rev (List.fold_left (fun acc x -> (first, x) :: acc) acc tail)
    in
    let rec _addpairs acc lst =
        match lst with
        | [] -> acc
        | hd :: tl -> (addsnd acc hd tl) @ _addpairs acc tl
    in _addpairs [] lst



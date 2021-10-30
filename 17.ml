let split (l: 'a list) (n: int): ('a list) * ('a list) =
    let rec _split accfirst left n_todo =
        if n_todo = 0 then 
            (accfirst, left)
        else
            match left with
            | [] -> (accfirst, [])
            | hd :: tl -> _split (hd :: accfirst) tl (n_todo - 1)
    in
    let x =_split [] l n
    in
    (List.rev (fst x), snd x)

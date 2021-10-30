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

let slice (l: 'a list) (a: int) (b: int): 'a list =
    let _, lslice = split l a 
    and n = List.length l in
    let _, rslice = split (List.rev lslice) (n - b - 1)
    in List.rev rslice

let rotate (l: 'a list) (n: int): 'a list =
    let n = if n >= 0 then n else (List.length l + n) in
    let left, tail = split l n in
    tail @ left

let remove_at (k: int) (l: 'a list): 'a list =
    let lslice, _ = split l k 
    and _, rslice = split l (k + 1)
    in lslice @ rslice

let insert_at (k: int) (c: 'a) (l: 'a list): 'a list =
    let lslice, _ = split l k
    and _, rslice = split l k
    in lslice @ [c] @ rslice

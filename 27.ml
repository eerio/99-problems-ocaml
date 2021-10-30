let rec skip lst k =
    if k = 0 then lst else 
    match lst with
    | [] -> []
    | hd :: tl -> skip tl (k - 1)

let rec group (lst: 'a list) (groups: int list): 'a list list list =
    let rec _group acc tail grps =
        match grps with
        | [] -> acc
        | hd :: tl -> (_group acc (List.tl tail) tl) :: acc
    in List.fold_left _group
        

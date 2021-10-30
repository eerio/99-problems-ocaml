let rec add_k acc x n =
    if n=0 then acc else add_k (x :: acc) x (n-1)

let replicate (l: 'a list) (n: int): 'a list =
    let _replicate acc x = add_k acc x n in
    List.rev (List.fold_left _replicate [] l)

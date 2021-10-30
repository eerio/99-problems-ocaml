

let rand_select (lst: 'a list) (n: int): 'a list =
    let rec _rand_select (acc: 'a list) (left_n: int) =
        if left_n = 0 then
            acc
        else 
            let rnd = List.nth lst (Random.int (List.length lst)) in
            rnd :: _rand_select acc (left_n - 1)
    in _rand_select [] n

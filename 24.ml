let lotto_select (k: int) (upto: int) =
    let rec _lotto (acc: int list) (todo: int) =
        if todo = 0 then
            acc
        else
            let rnd = Random.int (upto + 1) in
            

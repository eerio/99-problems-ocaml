type 'a rle =
    | One of 'a
    | Many of int * 'a

let rec add_k (l: 'a list) (x: 'a) (count: int) =
    if count = 0 then l else add_k (x :: l) x (count - 1)

let decode (l: 'a rle list): 'a list =
    let _decode acc elem =
        match elem with
        | One x -> x :: acc
        | Many (count, x) -> add_k acc x count
    in
    List.rev (List.fold_left _decode [] l)

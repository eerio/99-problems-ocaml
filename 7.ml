type 'a node =
    | One of 'a
    | Many of 'a node list

let rec flatten (l: 'a node list): 'a list =
    let flat_concat acc x =
        match x with
        | One (p) -> p :: acc
        | Many (ps) -> (List.rev (flatten ps)) @ acc
    in
    List.rev (List.fold_left flat_concat [] l)

let test = [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;

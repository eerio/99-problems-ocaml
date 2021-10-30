let rev (l: 'a list): 'a list =
    List.fold_left (fun acc x -> x :: acc) [] l

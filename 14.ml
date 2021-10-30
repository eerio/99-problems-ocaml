let duplicate (l: 'a list): 'a list =
    List.rev (List.fold_left (fun acc x -> x :: x :: acc) [] l)

let palindrome (l: 'a list): bool =
    List.for_all2 (fun x y -> x = y) l (List.rev l)


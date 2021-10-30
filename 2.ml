let rec last_two (l: 'a list): 'a list option =
    match l with
    | [] -> None
    | [fst; snd] as tl -> Some tl
    | hd :: tl -> last_two tl

let rec last (l: 'a list): 'a option =
    match l with
    | [] -> None
    | [hd] -> Some hd
    | hd :: tl -> last tl



let desc start = Stream.from (fun n -> Some (start - n))
let asc start = Stream.from (fun n -> Some (start + n))

let range a b =
    let ints = if b >= a then asc a else desc a
    in Stream.npeek (abs (b-a) + 1) ints

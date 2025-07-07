let two_sum nums target =
  let n = List.length nums in
  let rec outer i =
    if i >= n then (-1,-1)
    else
      let rec inner j =
        if j >= n then outer (i+1)
        else if List.nth nums i + List.nth nums j = target then (i,j)
        else inner (j+1)
      in
      inner (i+1)
  in
  outer 0

let () =
  let i,j = two_sum [2;7;11;15] 9 in
  Printf.printf "%d\n%d\n" i j

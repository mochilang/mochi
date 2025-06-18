exception Return_0 of string
let rec convert s numRows =
  try
    if numRows <= 1 || numRows >= String.length s then begin
      raise (Return_0 (s))
    end;
    let rows = ref [] in
    let i = ref 0 in
    while !i < numRows do
      rows := !rows @ [""];
      i := !i + 1;
    done;
    let curr = ref 0 in
    let step = ref 1 in
    String.iter (fun ch ->
      let rec _set_nth lst i v =
        match lst with
          | [] -> []
          | x::xs ->
            if i = 0 then v :: xs else x :: _set_nth xs (i - 1) v
        
      rows := _set_nth !rows !curr (List.nth !rows !curr) + ch;
      if !curr = 0 then begin
        step := 1;
      end else begin
        if !curr = numRows - 1 then begin
          step := -1;
        end;
      end;
      curr := !curr + !step;
    ) s;
    let result = ref "" in
    List.iter (fun row ->
      result := !result + row;
    ) !rows;
    raise (Return_0 (!result))
  with Return_0 v -> v



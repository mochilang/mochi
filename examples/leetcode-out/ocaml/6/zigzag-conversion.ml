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
      let tmp_1 = Array.of_list !rows in
      tmp_1.(!curr) <- (List.nth !rows !curr) ^ (String.make 1 ch);
      rows := Array.to_list tmp_1;
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
      result := !result ^ row;
    ) !rows;
    raise (Return_0 (!result))
  with Return_0 v -> v


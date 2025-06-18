exception Return_0 of bool
let rec isMatch s p =
  try
    let m = String.length s in
    let n = String.length p in
    let memo = ref (let tbl = Hashtbl.create 0 in  tbl) in
    exception Return_1 of bool
    let rec dfs i j =
      try
        let key = i * (n + 1) + j in
        if Hashtbl.mem !memo key then begin
          raise (Return_1 ((List.nth !memo key)))
        end;
        if j = n then begin
          raise (Return_1 (i = m))
        end;
        let first = ref false in
        if i < m then begin
          if ((String.get p j) = (String.get s i)) || ((String.get p j) = ".") then begin
            first := true;
          end;
        end;
        let ans = ref false in
        if j + 1 < n then begin
          if (String.get p j + 1) = "*" then begin
            if dfs i j + 2 then begin
              ans := true;
            end else begin
              if !first && dfs i + 1 j then begin
                ans := true;
              end;
            end;
          end else begin
            if !first && dfs i + 1 j + 1 then begin
              ans := true;
            end;
          end;
        end else begin
          if !first && dfs i + 1 j + 1 then begin
            ans := true;
          end;
        end;
        Hashtbl.replace !memo key !ans;
        raise (Return_1 (!ans))
      with Return_1 v -> v
    in
    raise (Return_0 (dfs 0 0))
  with Return_0 v -> v


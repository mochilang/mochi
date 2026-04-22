program Main;

uses SysUtils;

type TIntArray = array of LongInt; TBoolArray = array of Boolean;

function Max(a, b: LongInt): LongInt;
begin
  if a > b then Max := a else Max := b;
end;

function Solve(vals: TIntArray; ok: TBoolArray): LongInt;
var best: LongInt;
  function Dfs(i: LongInt): LongInt;
  var left, right, total: LongInt;
  begin
    if (i >= Length(vals)) or (not ok[i]) then begin Dfs := 0; Exit; end;
    left := Max(0, Dfs(2 * i + 1));
    right := Max(0, Dfs(2 * i + 2));
    total := vals[i] + left + right;
    if total > best then best := total;
    Dfs := vals[i] + Max(left, right);
  end;
begin
  best := -1000000000;
  Dfs(0);
  Solve := best;
end;

var tc, t, n, i: LongInt; tok: AnsiString; vals: TIntArray; ok: TBoolArray;
begin
  if EOF then Halt(0);
  ReadLn(tc);
  for t := 1 to tc do begin
    ReadLn(n);
    SetLength(vals, n);
    SetLength(ok, n);
    for i := 0 to n - 1 do begin
      ReadLn(tok);
      if tok = 'null' then begin vals[i] := 0; ok[i] := False; end
      else begin vals[i] := StrToInt(tok); ok[i] := True; end;
    end;
    Write(Solve(vals, ok));
    if t < tc then WriteLn;
  end;
end.

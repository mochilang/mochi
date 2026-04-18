program LongestCommonPrefix;

function StartsWith(s, p: String): Boolean;
begin
  StartsWith := Copy(s, 1, Length(p)) = p;
end;

function Lcp(strs: array of String): String;
var
  prefix: String;
  i: LongInt;
  ok: Boolean;
begin
  prefix := strs[0];
  while True do
  begin
    ok := True;
    for i := 0 to High(strs) do
      if not StartsWith(strs[i], prefix) then ok := False;
    if ok then Break;
    Delete(prefix, Length(prefix), 1);
  end;
  Lcp := prefix;
end;

var
  t, n, tc, i: LongInt;
  strs: array of String;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do
  begin
    ReadLn(n);
    SetLength(strs, n);
    for i := 0 to n - 1 do ReadLn(strs[i]);
    WriteLn('"', Lcp(strs), '"');
  end;
end.

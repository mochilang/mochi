program Main;
uses SysUtils;
function SolveCase(s: AnsiString): Integer;
var stack: array[0..40010] of Integer; top, i, best: Integer;
begin top := 0; stack[top] := -1; Inc(top); best := 0; for i := 1 to Length(s) do begin if s[i] = '(' then begin stack[top] := i - 1; Inc(top); end else begin Dec(top); if top = 0 then begin stack[top] := i - 1; Inc(top); end else if (i - 1 - stack[top-1]) > best then best := i - 1 - stack[top-1]; end; end; SolveCase := best; end;
var t, tc, n: Integer; s: AnsiString;
begin if EOF then Halt(0); ReadLn(t); for tc := 1 to t do begin ReadLn(n); if n > 0 then ReadLn(s) else s := ''; Write(SolveCase(s)); if tc < t then WriteLn; end; end.

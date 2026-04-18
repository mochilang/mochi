program Main;
uses SysUtils;
type TSol = array of AnsiString; TRes = array of TSol;
var n: LongInt;
procedure AppendSolution(var res: TRes; board: array of AnsiString); var i, len: LongInt; begin len := Length(res); SetLength(res, len + 1); SetLength(res[len], Length(board)); for i := 0 to High(board) do res[len][i] := board[i]; end;
procedure DFS(r, n: LongInt; var cols, d1, d2: array of Boolean; var board: array of AnsiString; var res: TRes); var c,a,b: LongInt; begin if r = n then begin AppendSolution(res, board); Exit; end; for c := 0 to n - 1 do begin a := r + c; b := r - c + n - 1; if cols[c] or d1[a] or d2[b] then Continue; cols[c] := True; d1[a] := True; d2[b] := True; board[r] := StringOfChar('.', c) + 'Q' + StringOfChar('.', n - c - 1); DFS(r + 1, n, cols, d1, d2, board, res); board[r] := StringOfChar('.', n); cols[c] := False; d1[a] := False; d2[b] := False; end; end;
function Solve(n: LongInt): TRes; var cols,d1,d2: array of Boolean; board: array of AnsiString; res: TRes; i: LongInt; begin SetLength(cols, n); SetLength(d1, 2*n); SetLength(d2, 2*n); SetLength(board, n); for i := 0 to n - 1 do board[i] := StringOfChar('.', n); SetLength(res, 0); DFS(0, n, cols, d1, d2, board, res); Solve := res; end;
var t,tc,i,j: LongInt; res: TRes;
begin if EOF then Halt(0); ReadLn(t); for tc := 1 to t do begin ReadLn(n); res := Solve(n); if tc > 1 then WriteLn('='); WriteLn(Length(res)); for i := 0 to High(res) do begin for j := 0 to High(res[i]) do WriteLn(res[i][j]); if i < High(res) then WriteLn('-'); end; end; end.

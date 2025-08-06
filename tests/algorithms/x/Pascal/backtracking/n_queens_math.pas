{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
type StrArrayArray = array of StrArray;
var _nowSeed: int64 = 0;
var _nowSeeded: boolean = false;
procedure init_now();
var s: string; v: int64;
begin
  s := GetEnvironmentVariable('MOCHI_NOW_SEED');
  if s <> '' then begin
    Val(s, v);
    _nowSeed := v;
    _nowSeeded := true;
  end;
end;
function _now(): integer;
begin
  if _nowSeeded then begin
    _nowSeed := (_nowSeed * 1664525 + 1013904223) mod 2147483647;
    _now := _nowSeed;
  end else begin
    _now := Integer(GetTickCount64()*1000);
  end;
end;
function _bench_now(): int64;
begin
  _bench_now := GetTickCount64()*1000;
end;
function _mem(): int64;
var h: TFPCHeapStatus;
begin
  h := GetFPCHeapStatus;
  _mem := h.CurrHeapUsed;
end;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
function contains(xs: array of integer; v: integer): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function contains(xs: IntArray; x: integer): boolean; forward;
function repeat_(s: string; times: integer): string; forward;
function build_board(pos: IntArray; n: integer): StrArray; forward;
function depth_first_search(pos: IntArray; dr: IntArray; dl: IntArray; n: integer): StrArrayArray; forward;
function n_queens_solution(n: integer): integer; forward;
function contains(xs: IntArray; x: integer): boolean;
var
  contains_i: integer;
begin
  contains_i := 0;
  while contains_i < Length(xs) do begin
  if xs[contains_i] = x then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function repeat_(s: string; times: integer): string;
var
  repeat__result_: string;
  repeat__i: integer;
begin
  repeat__result_ := '';
  repeat__i := 0;
  while repeat__i < times do begin
  repeat__result_ := repeat__result_ + s;
  repeat__i := repeat__i + 1;
end;
  exit(repeat__result_);
end;
function build_board(pos: IntArray; n: integer): StrArray;
var
  build_board_board: array of string;
  build_board_i: integer;
  build_board_col: integer;
  build_board_line: string;
begin
  build_board_board := [];
  build_board_i := 0;
  while build_board_i < Length(pos) do begin
  build_board_col := pos[build_board_i];
  build_board_line := (repeat_('. ', build_board_col) + 'Q ') + repeat_('. ', (n - 1) - build_board_col);
  build_board_board := concat(build_board_board, [build_board_line]);
  build_board_i := build_board_i + 1;
end;
  exit(build_board_board);
end;
function depth_first_search(pos: IntArray; dr: IntArray; dl: IntArray; n: integer): StrArrayArray;
var
  depth_first_search_row: integer;
  depth_first_search_single: array of StrArray;
  depth_first_search_boards: array of StrArray;
  depth_first_search_col: integer;
  depth_first_search_result_: array of StrArray;
begin
  depth_first_search_row := Length(pos);
  if depth_first_search_row = n then begin
  depth_first_search_single := [];
  depth_first_search_single := concat(depth_first_search_single, [build_board(pos, n)]);
  exit(depth_first_search_single);
end;
  depth_first_search_boards := [];
  depth_first_search_col := 0;
  while depth_first_search_col < n do begin
  if (contains(pos, depth_first_search_col) or contains(dr, depth_first_search_row - depth_first_search_col)) or contains(dl, depth_first_search_row + depth_first_search_col) then begin
  depth_first_search_col := depth_first_search_col + 1;
  continue;
end;
  depth_first_search_result_ := depth_first_search(concat(pos, [depth_first_search_col]), concat(dr, [depth_first_search_row - depth_first_search_col]), concat(dl, [depth_first_search_row + depth_first_search_col]), n);
  depth_first_search_boards := concat(depth_first_search_boards, depth_first_search_result_);
  depth_first_search_col := depth_first_search_col + 1;
end;
  exit(depth_first_search_boards);
end;
function n_queens_solution(n: integer): integer;
var
  n_queens_solution_boards: StrArrayArray;
  n_queens_solution_i: integer;
  n_queens_solution_j: integer;
begin
  n_queens_solution_boards := depth_first_search(IntArray([]), IntArray([]), IntArray([]), n);
  n_queens_solution_i := 0;
  while n_queens_solution_i < Length(n_queens_solution_boards) do begin
  n_queens_solution_j := 0;
  while n_queens_solution_j < Length(n_queens_solution_boards[n_queens_solution_i]) do begin
  writeln(n_queens_solution_boards[n_queens_solution_i][n_queens_solution_j]);
  n_queens_solution_j := n_queens_solution_j + 1;
end;
  writeln('');
  n_queens_solution_i := n_queens_solution_i + 1;
end;
  writeln(Length(n_queens_solution_boards), ' ', 'solutions were found.');
  exit(Length(n_queens_solution_boards));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  n_queens_solution(4);
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

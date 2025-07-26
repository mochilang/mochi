{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  dim: integer;
  newPile_b: array of IntArray;
  newPile_y: integer;
  newPile_row: array of integer;
  newPile_x: integer;
  drawPile_chars: array of string;
  drawPile_row: integer;
  drawPile_line: string;
  drawPile_col: integer;
  drawPile_v: integer;
  main_pile: IntArrayArray;
  main_hdim: integer;
function newPile(d: integer): IntArrayArray; forward;
function handlePile(pile: IntArrayArray; x: integer; y: integer): IntArrayArray; forward;
procedure drawPile(pile: IntArrayArray; d: integer); forward;
procedure main(); forward;
function newPile(d: integer): IntArrayArray;
begin
  newPile_b := [];
  newPile_y := 0;
  while newPile_y < d do begin
  newPile_row := [];
  newPile_x := 0;
  while newPile_x < d do begin
  newPile_row := concat(newPile_row, [0]);
  newPile_x := newPile_x + 1;
end;
  newPile_b := concat(newPile_b, [newPile_row]);
  newPile_y := newPile_y + 1;
end;
  exit(newPile_b);
end;
function handlePile(pile: IntArrayArray; x: integer; y: integer): IntArrayArray;
begin
  if pile[y][x] >= 4 then begin
  pile[y][x] := pile[y][x] - 4;
  if y > 0 then begin
  pile[y - 1][x] := pile[y - 1][x] + 1;
  if pile[y - 1][x] >= 4 then begin
  pile := handlePile(pile, x, y - 1);
end;
end;
  if x > 0 then begin
  pile[y][x - 1] := pile[y][x - 1] + 1;
  if pile[y][x - 1] >= 4 then begin
  pile := handlePile(pile, x - 1, y);
end;
end;
  if y < (dim - 1) then begin
  pile[y + 1][x] := pile[y + 1][x] + 1;
  if pile[y + 1][x] >= 4 then begin
  pile := handlePile(pile, x, y + 1);
end;
end;
  if x < (dim - 1) then begin
  pile[y][x + 1] := pile[y][x + 1] + 1;
  if pile[y][x + 1] >= 4 then begin
  pile := handlePile(pile, x + 1, y);
end;
end;
  pile := handlePile(pile, x, y);
end;
  exit(pile);
end;
procedure drawPile(pile: IntArrayArray; d: integer);
begin
  drawPile_chars := [' ', '░', '▓', '█'];
  drawPile_row := 0;
  while drawPile_row < d do begin
  drawPile_line := '';
  drawPile_col := 0;
  while drawPile_col < d do begin
  drawPile_v := pile[drawPile_row][drawPile_col];
  if drawPile_v > 3 then begin
  drawPile_v := 3;
end;
  drawPile_line := drawPile_line + drawPile_chars[drawPile_v];
  drawPile_col := drawPile_col + 1;
end;
  writeln(drawPile_line);
  drawPile_row := drawPile_row + 1;
end;
end;
procedure main();
begin
  main_pile := newPile(16);
  main_hdim := 7;
  main_pile[main_hdim][main_hdim] := 16;
  main_pile := handlePile(main_pile, main_hdim, main_hdim);
  drawPile(main_pile, 16);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  dim := 16;
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type IntArrayArrayArray = array of IntArrayArray;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
procedure show_list_list(xs: array of IntArray);
var i: integer;
begin
  for i := 0 to High(xs) do begin
    show_list(xs[i]);
    if i < High(xs) then write(' ');
  end;
  writeln('');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  GLIDER: array of IntArray;
  BLINKER: array of IntArray;
  frames: integer;
  cells: IntArrayArray;
function new_generation(cells: IntArrayArray): IntArrayArray; forward;
function generate_generations(cells: IntArrayArray; frames: integer): IntArrayArrayArray; forward;
procedure main(); forward;
function new_generation(cells: IntArrayArray): IntArrayArray;
var
  new_generation_rows: integer;
  new_generation_cols: integer;
  new_generation_next: array of IntArray;
  new_generation_i: integer;
  new_generation_row: array of integer;
  new_generation_j: integer;
  new_generation_count: integer;
  new_generation_alive: boolean;
begin
  new_generation_rows := Length(cells);
  new_generation_cols := Length(cells[0]);
  new_generation_next := [];
  new_generation_i := 0;
  while new_generation_i < new_generation_rows do begin
  new_generation_row := [];
  new_generation_j := 0;
  while new_generation_j < new_generation_cols do begin
  new_generation_count := 0;
  if (new_generation_i > 0) and (new_generation_j > 0) then begin
  new_generation_count := new_generation_count + cells[new_generation_i - 1][new_generation_j - 1];
end;
  if new_generation_i > 0 then begin
  new_generation_count := new_generation_count + cells[new_generation_i - 1][new_generation_j];
end;
  if (new_generation_i > 0) and (new_generation_j < (new_generation_cols - 1)) then begin
  new_generation_count := new_generation_count + cells[new_generation_i - 1][new_generation_j + 1];
end;
  if new_generation_j > 0 then begin
  new_generation_count := new_generation_count + cells[new_generation_i][new_generation_j - 1];
end;
  if new_generation_j < (new_generation_cols - 1) then begin
  new_generation_count := new_generation_count + cells[new_generation_i][new_generation_j + 1];
end;
  if (new_generation_i < (new_generation_rows - 1)) and (new_generation_j > 0) then begin
  new_generation_count := new_generation_count + cells[new_generation_i + 1][new_generation_j - 1];
end;
  if new_generation_i < (new_generation_rows - 1) then begin
  new_generation_count := new_generation_count + cells[new_generation_i + 1][new_generation_j];
end;
  if (new_generation_i < (new_generation_rows - 1)) and (new_generation_j < (new_generation_cols - 1)) then begin
  new_generation_count := new_generation_count + cells[new_generation_i + 1][new_generation_j + 1];
end;
  new_generation_alive := cells[new_generation_i][new_generation_j] = 1;
  if ((new_generation_alive and (new_generation_count >= 2)) and (new_generation_count <= 3)) or (not new_generation_alive and (new_generation_count = 3)) then begin
  new_generation_row := concat(new_generation_row, IntArray([1]));
end else begin
  new_generation_row := concat(new_generation_row, IntArray([0]));
end;
  new_generation_j := new_generation_j + 1;
end;
  new_generation_next := concat(new_generation_next, [new_generation_row]);
  new_generation_i := new_generation_i + 1;
end;
  exit(new_generation_next);
end;
function generate_generations(cells: IntArrayArray; frames: integer): IntArrayArrayArray;
var
  generate_generations_result_: array of IntArrayArray;
  generate_generations_i: integer;
  generate_generations_current: array of IntArray;
begin
  generate_generations_result_ := [];
  generate_generations_i := 0;
  generate_generations_current := cells;
  while generate_generations_i < frames do begin
  generate_generations_result_ := concat(generate_generations_result_, [generate_generations_current]);
  generate_generations_current := new_generation(generate_generations_current);
  generate_generations_i := generate_generations_i + 1;
end;
  exit(generate_generations_result_);
end;
procedure main();
var
  main_frames: IntArrayArrayArray;
  main_i: integer;
begin
  main_frames := generate_generations(GLIDER, 4);
  main_i := 0;
  while main_i < Length(main_frames) do begin
  show_list_list(main_frames[main_i]);
  main_i := main_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  GLIDER := [[0, 1, 0, 0, 0, 0, 0, 0], [0, 0, 1, 0, 0, 0, 0, 0], [1, 1, 1, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0]];
  BLINKER := [[0, 1, 0], [0, 1, 0], [0, 1, 0]];
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

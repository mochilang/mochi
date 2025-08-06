{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function minimax(depth: integer; node_index: integer; is_max: boolean; scores: IntArray; height: integer): integer; forward;
function tree_height(n: integer): integer; forward;
procedure main(); forward;
function minimax(depth: integer; node_index: integer; is_max: boolean; scores: IntArray; height: integer): integer;
var
  minimax_left: integer;
  minimax_right: integer;
begin
  if depth < 0 then begin
  panic('Depth cannot be less than 0');
end;
  if Length(scores) = 0 then begin
  panic('Scores cannot be empty');
end;
  if depth = height then begin
  exit(scores[node_index]);
end;
  if is_max then begin
  minimax_left := minimax(depth + 1, node_index * 2, false, scores, height);
  minimax_right := minimax(depth + 1, (node_index * 2) + 1, false, scores, height);
  if minimax_left > minimax_right then begin
  exit(minimax_left);
end else begin
  exit(minimax_right);
end;
end;
  minimax_left := minimax(depth + 1, node_index * 2, true, scores, height);
  minimax_right := minimax(depth + 1, (node_index * 2) + 1, true, scores, height);
  if minimax_left < minimax_right then begin
  exit(minimax_left);
end else begin
  exit(minimax_right);
end;
end;
function tree_height(n: integer): integer;
var
  tree_height_h: integer;
  tree_height_v: integer;
begin
  tree_height_h := 0;
  tree_height_v := n;
  while tree_height_v > 1 do begin
  tree_height_v := tree_height_v div 2;
  tree_height_h := tree_height_h + 1;
end;
  exit(tree_height_h);
end;
procedure main();
var
  main_scores: array of integer;
  main_height: integer;
begin
  main_scores := [90, 23, 6, 33, 21, 65, 123, 34423];
  main_height := tree_height(Length(main_scores));
  writeln('Optimal value : ' + IntToStr(minimax(0, 0, true, main_scores, main_height)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
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

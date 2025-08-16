{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type RealArray = array of real;
type IntArray = array of integer;
type RealArrayArray = array of RealArray;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  alpha: real;
  xs: RealArray;
  m: RealArrayArray;
  sample: IntArray;
  weights: RealArrayArray;
  j: integer;
function get_winner(weights: RealArrayArray; sample: IntArray): integer; forward;
function update(weights: RealArrayArray; sample: IntArray; j: integer; alpha: real): RealArrayArray; forward;
function list_to_string(xs: RealArray): string; forward;
function matrix_to_string(m: RealArrayArray): string; forward;
procedure main(); forward;
function get_winner(weights: RealArrayArray; sample: IntArray): integer;
var
  get_winner_d0: real;
  get_winner_d1: real;
  get_winner_i: int64;
  get_winner_diff0: real;
  get_winner_diff1: real;
begin
  get_winner_d0 := 0;
  get_winner_d1 := 0;
  for get_winner_i := 0 to (Length(sample) - 1) do begin
  get_winner_diff0 := sample[get_winner_i] - weights[0][get_winner_i];
  get_winner_diff1 := sample[get_winner_i] - weights[1][get_winner_i];
  get_winner_d0 := get_winner_d0 + (get_winner_diff0 * get_winner_diff0);
  get_winner_d1 := get_winner_d1 + (get_winner_diff1 * get_winner_diff1);
  exit(IfThen(get_winner_d0 > get_winner_d1, 0, 1));
end;
  exit(0);
end;
function update(weights: RealArrayArray; sample: IntArray; j: integer; alpha: real): RealArrayArray;
var
  update_i: int64;
begin
  for update_i := 0 to (Length(weights) - 1) do begin
  weights[j][update_i] := weights[j][update_i] + (alpha * (sample[update_i] - weights[j][update_i]));
end;
  exit(weights);
end;
function list_to_string(xs: RealArray): string;
var
  list_to_string_s: string;
  list_to_string_i: integer;
begin
  list_to_string_s := '[';
  list_to_string_i := 0;
  while list_to_string_i < Length(xs) do begin
  list_to_string_s := list_to_string_s + FloatToStr(xs[list_to_string_i]);
  if list_to_string_i < (Length(xs) - 1) then begin
  list_to_string_s := list_to_string_s + ', ';
end;
  list_to_string_i := list_to_string_i + 1;
end;
  list_to_string_s := list_to_string_s + ']';
  exit(list_to_string_s);
end;
function matrix_to_string(m: RealArrayArray): string;
var
  matrix_to_string_s: string;
  matrix_to_string_i: integer;
begin
  matrix_to_string_s := '[';
  matrix_to_string_i := 0;
  while matrix_to_string_i < Length(m) do begin
  matrix_to_string_s := matrix_to_string_s + list_to_string(m[matrix_to_string_i]);
  if matrix_to_string_i < (Length(m) - 1) then begin
  matrix_to_string_s := matrix_to_string_s + ', ';
end;
  matrix_to_string_i := matrix_to_string_i + 1;
end;
  matrix_to_string_s := matrix_to_string_s + ']';
  exit(matrix_to_string_s);
end;
procedure main();
var
  main_training_samples: array of IntArray;
  main_weights: array of RealArray;
  main_epochs: integer;
  main_alpha: real;
  main__: int64;
  main_j: int64;
  main_sample: array of integer;
  main_winner: integer;
begin
  main_training_samples := [[1, 1, 0, 0], [0, 0, 0, 1], [1, 0, 0, 0], [0, 0, 1, 1]];
  main_weights := [[0.2, 0.6, 0.5, 0.9], [0.8, 0.4, 0.7, 0.3]];
  main_epochs := 3;
  main_alpha := 0.5;
  for main__ := 0 to (main_epochs - 1) do begin
  for main_j := 0 to (Length(main_training_samples) - 1) do begin
  main_sample := main_training_samples[main_j];
  main_winner := get_winner(main_weights, main_sample);
  main_weights := update(main_weights, main_sample, main_winner, main_alpha);
end;
end;
  main_sample := [0, 0, 0, 1];
  main_winner := get_winner(main_weights, main_sample);
  writeln('Clusters that the test sample belongs to : ' + IntToStr(main_winner));
  writeln('Weights that have been trained : ' + matrix_to_string(main_weights));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

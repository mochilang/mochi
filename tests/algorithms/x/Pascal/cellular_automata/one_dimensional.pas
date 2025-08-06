{$mode objfpc}
program Main;
uses SysUtils, Math;
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
  initial: array of integer;
  cells: array of IntArray;
  rules: IntArray;
  time: integer;
  next: IntArray;
  t: integer;
  row: IntArray;
  ruleset: integer;
function format_ruleset(ruleset: integer): IntArray; forward;
function new_generation(cells: IntArrayArray; rules: IntArray; time: integer): IntArray; forward;
function cells_to_string(row: IntArray): string; forward;
function format_ruleset(ruleset: integer): IntArray;
var
  format_ruleset_rs: integer;
  format_ruleset_bits_rev: array of integer;
  format_ruleset_i: integer;
  format_ruleset_bits: array of integer;
  format_ruleset_j: integer;
begin
  format_ruleset_rs := ruleset;
  format_ruleset_bits_rev := [];
  format_ruleset_i := 0;
  while format_ruleset_i < 8 do begin
  format_ruleset_bits_rev := concat(format_ruleset_bits_rev, IntArray([format_ruleset_rs mod 2]));
  format_ruleset_rs := format_ruleset_rs div 2;
  format_ruleset_i := format_ruleset_i + 1;
end;
  format_ruleset_bits := [];
  format_ruleset_j := Length(format_ruleset_bits_rev) - 1;
  while format_ruleset_j >= 0 do begin
  format_ruleset_bits := concat(format_ruleset_bits, IntArray([format_ruleset_bits_rev[format_ruleset_j]]));
  format_ruleset_j := format_ruleset_j - 1;
end;
  exit(format_ruleset_bits);
end;
function new_generation(cells: IntArrayArray; rules: IntArray; time: integer): IntArray;
var
  new_generation_population: integer;
  new_generation_next_generation: array of integer;
  new_generation_i: integer;
  new_generation_left_neighbor: integer;
  new_generation_right_neighbor: integer;
  new_generation_center: integer;
  new_generation_idx: integer;
begin
  new_generation_population := Length(cells[0]);
  new_generation_next_generation := [];
  new_generation_i := 0;
  while new_generation_i < new_generation_population do begin
  new_generation_left_neighbor := IfThen(new_generation_i = 0, 0, cells[time][new_generation_i - 1]);
  new_generation_right_neighbor := IfThen(new_generation_i = (new_generation_population - 1), 0, cells[time][new_generation_i + 1]);
  new_generation_center := cells[time][new_generation_i];
  new_generation_idx := 7 - (((new_generation_left_neighbor * 4) + (new_generation_center * 2)) + new_generation_right_neighbor);
  new_generation_next_generation := concat(new_generation_next_generation, IntArray([rules[new_generation_idx]]));
  new_generation_i := new_generation_i + 1;
end;
  exit(new_generation_next_generation);
end;
function cells_to_string(row: IntArray): string;
var
  cells_to_string_result_: string;
  cells_to_string_i: integer;
begin
  cells_to_string_result_ := '';
  cells_to_string_i := 0;
  while cells_to_string_i < Length(row) do begin
  if row[cells_to_string_i] = 1 then begin
  cells_to_string_result_ := cells_to_string_result_ + '#';
end else begin
  cells_to_string_result_ := cells_to_string_result_ + '.';
end;
  cells_to_string_i := cells_to_string_i + 1;
end;
  exit(cells_to_string_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  initial := [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
  cells := [initial];
  rules := format_ruleset(30);
  time := 0;
  while time < 16 do begin
  next := new_generation(cells, rules, time);
  cells := concat(cells, [next]);
  time := time + 1;
end;
  t := 0;
  while t < Length(cells) do begin
  writeln(cells_to_string(cells[t]));
  t := t + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

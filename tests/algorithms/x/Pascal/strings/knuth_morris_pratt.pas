{$mode objfpc}{$modeswitch nestedprocvars}
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
  text: string;
  pattern: string;
function get_failure_array(get_failure_array_pattern: string): IntArray; forward;
function knuth_morris_pratt(knuth_morris_pratt_text: string; knuth_morris_pratt_pattern: string): integer; forward;
function get_failure_array(get_failure_array_pattern: string): IntArray;
var
  get_failure_array_failure: array of integer;
  get_failure_array_i: integer;
  get_failure_array_j: integer;
begin
  get_failure_array_failure := [0];
  get_failure_array_i := 0;
  get_failure_array_j := 1;
  while get_failure_array_j < Length(get_failure_array_pattern) do begin
  if copy(get_failure_array_pattern, get_failure_array_i+1, (get_failure_array_i + 1 - (get_failure_array_i))) = copy(get_failure_array_pattern, get_failure_array_j+1, (get_failure_array_j + 1 - (get_failure_array_j))) then begin
  get_failure_array_i := get_failure_array_i + 1;
end else begin
  if get_failure_array_i > 0 then begin
  get_failure_array_i := get_failure_array_failure[get_failure_array_i - 1];
  continue;
end;
end;
  get_failure_array_j := get_failure_array_j + 1;
  get_failure_array_failure := concat(get_failure_array_failure, [get_failure_array_i]);
end;
  exit(get_failure_array_failure);
end;
function knuth_morris_pratt(knuth_morris_pratt_text: string; knuth_morris_pratt_pattern: string): integer;
var
  knuth_morris_pratt_failure: IntArray;
  knuth_morris_pratt_i: integer;
  knuth_morris_pratt_j: integer;
begin
  knuth_morris_pratt_failure := get_failure_array(knuth_morris_pratt_pattern);
  knuth_morris_pratt_i := 0;
  knuth_morris_pratt_j := 0;
  while knuth_morris_pratt_i < Length(knuth_morris_pratt_text) do begin
  if copy(knuth_morris_pratt_pattern, knuth_morris_pratt_j+1, (knuth_morris_pratt_j + 1 - (knuth_morris_pratt_j))) = copy(knuth_morris_pratt_text, knuth_morris_pratt_i+1, (knuth_morris_pratt_i + 1 - (knuth_morris_pratt_i))) then begin
  if knuth_morris_pratt_j = (Length(knuth_morris_pratt_pattern) - 1) then begin
  exit(knuth_morris_pratt_i - knuth_morris_pratt_j);
end;
  knuth_morris_pratt_j := knuth_morris_pratt_j + 1;
end else begin
  if knuth_morris_pratt_j > 0 then begin
  knuth_morris_pratt_j := knuth_morris_pratt_failure[knuth_morris_pratt_j - 1];
  continue;
end;
end;
  knuth_morris_pratt_i := knuth_morris_pratt_i + 1;
end;
  exit(-1);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  text := 'abcxabcdabxabcdabcdabcy';
  pattern := 'abcdabcy';
  writeln(knuth_morris_pratt(text, pattern));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

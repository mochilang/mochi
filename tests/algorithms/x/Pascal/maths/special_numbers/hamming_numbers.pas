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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  n: integer;
function hamming(n: integer): IntArray; forward;
function hamming(n: integer): IntArray;
var
  hamming_hamming_list: array of integer;
  hamming_i: integer;
  hamming_j: integer;
  hamming_k: integer;
  hamming_index: integer;
  hamming_m1: integer;
  hamming_m2: integer;
  hamming_m3: integer;
  hamming_next: integer;
begin
  if n < 1 then begin
  panic('n_element should be a positive number');
end;
  hamming_hamming_list := [1];
  hamming_i := 0;
  hamming_j := 0;
  hamming_k := 0;
  hamming_index := 1;
  while hamming_index < n do begin
  while (hamming_hamming_list[hamming_i] * 2) <= hamming_hamming_list[Length(hamming_hamming_list) - 1] do begin
  hamming_i := hamming_i + 1;
end;
  while (hamming_hamming_list[hamming_j] * 3) <= hamming_hamming_list[Length(hamming_hamming_list) - 1] do begin
  hamming_j := hamming_j + 1;
end;
  while (hamming_hamming_list[hamming_k] * 5) <= hamming_hamming_list[Length(hamming_hamming_list) - 1] do begin
  hamming_k := hamming_k + 1;
end;
  hamming_m1 := hamming_hamming_list[hamming_i] * 2;
  hamming_m2 := hamming_hamming_list[hamming_j] * 3;
  hamming_m3 := hamming_hamming_list[hamming_k] * 5;
  hamming_next := hamming_m1;
  if hamming_m2 < hamming_next then begin
  hamming_next := hamming_m2;
end;
  if hamming_m3 < hamming_next then begin
  hamming_next := hamming_m3;
end;
  hamming_hamming_list := concat(hamming_hamming_list, IntArray([hamming_next]));
  hamming_index := hamming_index + 1;
end;
  exit(hamming_hamming_list);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_list(hamming(5));
  show_list(hamming(10));
  show_list(hamming(15));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

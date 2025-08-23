{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
function hamming_distance(hamming_distance_a: string; hamming_distance_b: string): integer; forward;
function hamming_distance(hamming_distance_a: string; hamming_distance_b: string): integer;
var
  hamming_distance_i: integer;
  hamming_distance_count: integer;
begin
  if Length(hamming_distance_a) <> Length(hamming_distance_b) then begin
  panic('String lengths must match!');
end;
  hamming_distance_i := 0;
  hamming_distance_count := 0;
  while hamming_distance_i < Length(hamming_distance_a) do begin
  if hamming_distance_a[hamming_distance_i+1] <> hamming_distance_b[hamming_distance_i+1] then begin
  hamming_distance_count := hamming_distance_count + 1;
end;
  hamming_distance_i := hamming_distance_i + 1;
end;
  exit(hamming_distance_count);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(hamming_distance('python', 'python')));
  writeln(IntToStr(hamming_distance('karolin', 'kathrin')));
  writeln(IntToStr(hamming_distance('00000', '11111')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

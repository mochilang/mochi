{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
  vector: RealArray;
function binary_step(vector: RealArray): IntArray; forward;
procedure main(); forward;
function binary_step(vector: RealArray): IntArray;
var
  binary_step_out_: array of integer;
  binary_step_i: integer;
begin
  binary_step_out_ := [];
  binary_step_i := 0;
  while binary_step_i < Length(vector) do begin
  if vector[binary_step_i] >= 0 then begin
  binary_step_out_ := concat(binary_step_out_, IntArray([1]));
end else begin
  binary_step_out_ := concat(binary_step_out_, IntArray([0]));
end;
  binary_step_i := binary_step_i + 1;
end;
  exit(binary_step_out_);
end;
procedure main();
var
  main_vector: array of real;
  main_result_: array of integer;
begin
  main_vector := [-1.2, 0, 2, 1.45, -3.7, 0.3];
  main_result_ := binary_step(main_vector);
  show_list(main_result_);
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

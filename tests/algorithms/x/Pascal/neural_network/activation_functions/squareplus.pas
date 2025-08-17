{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function sqrtApprox(sqrtApprox_x: real): real; forward;
function squareplus(squareplus_vector: RealArray; squareplus_beta: real): RealArray; forward;
procedure main(); forward;
function sqrtApprox(sqrtApprox_x: real): real;
var
  sqrtApprox_guess: real;
  sqrtApprox_i: integer;
begin
  if sqrtApprox_x <= 0 then begin
  exit(0);
end;
  sqrtApprox_guess := sqrtApprox_x;
  sqrtApprox_i := 0;
  while sqrtApprox_i < 20 do begin
  sqrtApprox_guess := (sqrtApprox_guess + (sqrtApprox_x / sqrtApprox_guess)) / 2;
  sqrtApprox_i := sqrtApprox_i + 1;
end;
  exit(sqrtApprox_guess);
end;
function squareplus(squareplus_vector: RealArray; squareplus_beta: real): RealArray;
var
  squareplus_result_: array of real;
  squareplus_i: integer;
  squareplus_x: real;
  squareplus_val: real;
begin
  squareplus_result_ := [];
  squareplus_i := 0;
  while squareplus_i < Length(squareplus_vector) do begin
  squareplus_x := squareplus_vector[squareplus_i];
  squareplus_val := (squareplus_x + sqrtApprox((squareplus_x * squareplus_x) + squareplus_beta)) / 2;
  squareplus_result_ := concat(squareplus_result_, [squareplus_val]);
  squareplus_i := squareplus_i + 1;
end;
  exit(squareplus_result_);
end;
procedure main();
var
  main_v1: array of real;
  main_v2: array of real;
begin
  main_v1 := [2.3, 0.6, -2, -3.8];
  main_v2 := [-9.2, -0.3, 0.45, -4.56];
  writeln(list_real_to_str(squareplus(main_v1, 2)));
  writeln(list_real_to_str(squareplus(main_v2, 3)));
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

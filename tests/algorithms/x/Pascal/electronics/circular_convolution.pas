{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
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
procedure json(x: int64);
begin
  writeln(x);
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
  example1: RealArray;
  example2: RealArray;
  example3: RealArray;
  example4: RealArray;
function floor(floor_x: real): real; forward;
function pow10(pow10_n: int64): real; forward;
function roundn(roundn_x: real; roundn_n: int64): real; forward;
function pad(pad_signal: RealArray; pad_target: int64): RealArray; forward;
function circular_convolution(circular_convolution_a: RealArray; circular_convolution_b: RealArray): RealArray; forward;
function floor(floor_x: real): real;
var
  floor_i: integer;
begin
  floor_i := Trunc(floor_x);
  if Double(floor_i) > floor_x then begin
  floor_i := floor_i - 1;
end;
  exit(Double(floor_i));
end;
function pow10(pow10_n: int64): real;
var
  pow10_p: real;
  pow10_i: int64;
begin
  pow10_p := 1;
  pow10_i := 0;
  while pow10_i < pow10_n do begin
  pow10_p := pow10_p * 10;
  pow10_i := pow10_i + 1;
end;
  exit(pow10_p);
end;
function roundn(roundn_x: real; roundn_n: int64): real;
var
  roundn_m: real;
begin
  roundn_m := pow10(roundn_n);
  exit(Floor((roundn_x * roundn_m) + 0.5) / roundn_m);
end;
function pad(pad_signal: RealArray; pad_target: int64): RealArray;
var
  pad_s: array of real;
begin
  pad_s := pad_signal;
  while Length(pad_s) < pad_target do begin
  pad_s := concat(pad_s, [0]);
end;
  exit(pad_s);
end;
function circular_convolution(circular_convolution_a: RealArray; circular_convolution_b: RealArray): RealArray;
var
  circular_convolution_n1: integer;
  circular_convolution_n2: integer;
  circular_convolution_n: integer;
  circular_convolution_x: RealArray;
  circular_convolution_y: RealArray;
  circular_convolution_res: array of real;
  circular_convolution_i: int64;
  circular_convolution_sum: real;
  circular_convolution_k: int64;
  circular_convolution_j: int64;
  circular_convolution_idx: int64;
begin
  circular_convolution_n1 := Length(circular_convolution_a);
  circular_convolution_n2 := Length(circular_convolution_b);
  if circular_convolution_n1 > circular_convolution_n2 then begin
  circular_convolution_n := circular_convolution_n1;
end else begin
  circular_convolution_n := circular_convolution_n2;
end;
  circular_convolution_x := pad(circular_convolution_a, circular_convolution_n);
  circular_convolution_y := pad(circular_convolution_b, circular_convolution_n);
  circular_convolution_res := [];
  circular_convolution_i := 0;
  while circular_convolution_i < circular_convolution_n do begin
  circular_convolution_sum := 0;
  circular_convolution_k := 0;
  while circular_convolution_k < circular_convolution_n do begin
  circular_convolution_j := (circular_convolution_i - circular_convolution_k) mod circular_convolution_n;
  if circular_convolution_j < 0 then begin
  circular_convolution_idx := circular_convolution_j + circular_convolution_n;
end else begin
  circular_convolution_idx := circular_convolution_j;
end;
  circular_convolution_sum := circular_convolution_sum + (circular_convolution_x[circular_convolution_k] * circular_convolution_y[circular_convolution_idx]);
  circular_convolution_k := circular_convolution_k + 1;
end;
  circular_convolution_res := concat(circular_convolution_res, [roundn(circular_convolution_sum, 2)]);
  circular_convolution_i := circular_convolution_i + 1;
end;
  exit(circular_convolution_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  example1 := circular_convolution([2, 1, 2, -1], [1, 2, 3, 4]);
  writeln(list_real_to_str(example1));
  example2 := circular_convolution([0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6], [0.1, 0.3, 0.5, 0.7, 0.9, 1.1, 1.3, 1.5]);
  writeln(list_real_to_str(example2));
  example3 := circular_convolution([-1, 1, 2, -2], [0.5, 1, -1, 2, 0.75]);
  writeln(list_real_to_str(example3));
  example4 := circular_convolution([1, -1, 2, 3, -1], [1, 2, 3]);
  writeln(list_real_to_str(example4));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

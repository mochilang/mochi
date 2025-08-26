{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, StrUtils, Math;
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
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  K: real;
function format2(format2_x: real): string; forward;
function coulombs_law(coulombs_law_q1: real; coulombs_law_q2: real; coulombs_law_radius: real): real; forward;
function format2(format2_x: real): string;
var
  format2_sign: string;
  format2_y: real;
  format2_m: real;
  format2_scaled: real;
  format2_i: integer;
  format2_int_part: integer;
  format2_frac_part: integer;
  format2_frac_str: string;
begin
  if format2_x < 0 then begin
  format2_sign := '-';
end else begin
  format2_sign := '';
end;
  if format2_x < 0 then begin
  format2_y := -format2_x;
end else begin
  format2_y := format2_x;
end;
  format2_m := 100;
  format2_scaled := format2_y * format2_m;
  format2_i := Trunc(format2_scaled);
  if (format2_scaled - Double(format2_i)) >= 0.5 then begin
  format2_i := format2_i + 1;
end;
  format2_int_part := _floordiv(format2_i, 100);
  format2_frac_part := format2_i mod 100;
  format2_frac_str := IntToStr(format2_frac_part);
  if format2_frac_part < 10 then begin
  format2_frac_str := '0' + format2_frac_str;
end;
  exit(((format2_sign + IntToStr(format2_int_part)) + '.') + format2_frac_str);
end;
function coulombs_law(coulombs_law_q1: real; coulombs_law_q2: real; coulombs_law_radius: real): real;
var
  coulombs_law_force: real;
begin
  if coulombs_law_radius <= 0 then begin
  panic('radius must be positive');
end;
  coulombs_law_force := ((K * coulombs_law_q1) * coulombs_law_q2) / (coulombs_law_radius * coulombs_law_radius);
  exit(coulombs_law_force);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  K := 8.9875517923e+09;
  writeln(format2(coulombs_law(15.5, 20, 15)));
  writeln(format2(coulombs_law(1, 15, 5)));
  writeln(format2(coulombs_law(20, -50, 15)));
  writeln(format2(coulombs_law(-5, -8, 10)));
  writeln(format2(coulombs_law(50, 100, 50)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

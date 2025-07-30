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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function primeFactors(n: integer): IntArray; forward;
function repeat(ch: string; n: integer): string; forward;
function D(n: real): real; forward;
function pad(n: integer): string; forward;
procedure main(); forward;
function primeFactors(n: integer): IntArray;
var
  primeFactors_factors: array of integer;
  primeFactors_x: integer;
  primeFactors_p: integer;
begin
  primeFactors_factors := [];
  primeFactors_x := n;
  while (primeFactors_x mod 2) = 0 do begin
  primeFactors_factors := concat(primeFactors_factors, [2]);
  primeFactors_x := Trunc(primeFactors_x div 2);
end;
  primeFactors_p := 3;
  while (primeFactors_p * primeFactors_p) <= primeFactors_x do begin
  while (primeFactors_x mod primeFactors_p) = 0 do begin
  primeFactors_factors := concat(primeFactors_factors, [primeFactors_p]);
  primeFactors_x := Trunc(primeFactors_x div primeFactors_p);
end;
  primeFactors_p := primeFactors_p + 2;
end;
  if primeFactors_x > 1 then begin
  primeFactors_factors := concat(primeFactors_factors, [primeFactors_x]);
end;
  exit(primeFactors_factors);
end;
function repeat(ch: string; n: integer): string;
var
  repeat_s: string;
  repeat_i: integer;
begin
  repeat_s := '';
  repeat_i := 0;
  while repeat_i < n do begin
  repeat_s := repeat_s + ch;
  repeat_i := repeat_i + 1;
end;
  exit(repeat_s);
end;
function D(n: real): real;
var
  D_factors: array of integer;
  D_g: integer;
  D_c: integer;
  D_d: real;
begin
  if n < 0 then begin
  exit(-D(-n));
end;
  if n < 2 then begin
  exit(0);
end;
  D_factors := [];
  if n < 1e+19 then begin
  D_factors := primeFactors(Trunc(n));
end else begin
  D_g := Trunc(n / 100);
  D_factors := primeFactors(D_g);
  D_factors := concat(D_factors, [2]);
  D_factors := concat(D_factors, [2]);
  D_factors := concat(D_factors, [5]);
  D_factors := concat(D_factors, [5]);
end;
  D_c := Length(D_factors);
  if D_c = 1 then begin
  exit(1);
end;
  if D_c = 2 then begin
  exit(Double(D_factors[0] + D_factors[1]));
end;
  D_d := n / Double(D_factors[0]);
  exit((D(D_d) * Double(D_factors[0])) + D_d);
end;
function pad(n: integer): string;
var
  pad_s: string;
begin
  pad_s := IntToStr(n);
  while Length(pad_s) < 4 do begin
  pad_s := ' ' + pad_s;
end;
  exit(pad_s);
end;
procedure main();
var
  main_vals: array of integer;
  main_n: integer;
  main_i: integer;
  main_line: string;
  main_j: integer;
  main_pow: real;
  main_m: integer;
  main_exp: string;
  main_res: string;
begin
  main_vals := [];
  main_n := -99;
  while main_n < 101 do begin
  main_vals := concat(main_vals, [Trunc(D(Double(main_n)))]);
  main_n := main_n + 1;
end;
  main_i := 0;
  while main_i < Length(main_vals) do begin
  main_line := '';
  main_j := 0;
  while main_j < 10 do begin
  main_line := main_line + pad(main_vals[main_i + main_j]);
  if main_j < 9 then begin
  main_line := main_line + ' ';
end;
  main_j := main_j + 1;
end;
  writeln(main_line);
  main_i := main_i + 10;
end;
  main_pow := 1;
  main_m := 1;
  while main_m < 21 do begin
  main_pow := main_pow * 10;
  main_exp := IntToStr(main_m);
  if Length(main_exp) < 2 then begin
  main_exp := main_exp + ' ';
end;
  main_res := IntToStr(main_m) + repeat('0', main_m - 1);
  writeln((('D(10^' + main_exp) + ') / 7 = ') + main_res);
  main_m := main_m + 1;
end;
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

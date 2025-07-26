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
  divisors_divs: array of integer;
  divisors_divs2: array of integer;
  divisors_i: integer;
  divisors_j: integer;
  sum_tot: integer;
  v: integer;
  sumStr_s: string;
  sumStr_i: integer;
  pad2_s: string;
  pad5_s: string;
  abundantOdd_count: integer;
  abundantOdd_n: integer;
  abundantOdd_divs: IntArray;
  abundantOdd_tot: integer;
  abundantOdd_s: string;
  main_max: integer;
  main_n: integer;
function divisors(n: integer): IntArray; forward;
function sum(xs: IntArray): integer; forward;
function sumStr(xs: IntArray): string; forward;
function pad2(n: integer): string; forward;
function pad5(n: integer): string; forward;
function abundantOdd(searchFrom: integer; countFrom: integer; countTo: integer; printOne: boolean): integer; forward;
procedure main(); forward;
function divisors(n: integer): IntArray;
begin
  divisors_divs := [1];
  divisors_divs2 := [];
  divisors_i := 2;
  while (divisors_i * divisors_i) <= n do begin
  if (n mod divisors_i) = 0 then begin
  divisors_j := Trunc(n div divisors_i);
  divisors_divs := concat(divisors_divs, [divisors_i]);
  if divisors_i <> divisors_j then begin
  divisors_divs2 := concat(divisors_divs2, [divisors_j]);
end;
end;
  divisors_i := divisors_i + 1;
end;
  divisors_j := Length(divisors_divs2) - 1;
  while divisors_j >= 0 do begin
  divisors_divs := concat(divisors_divs, [divisors_divs2[divisors_j]]);
  divisors_j := divisors_j - 1;
end;
  exit(divisors_divs);
end;
function sum(xs: IntArray): integer;
begin
  sum_tot := 0;
  for v in xs do begin
  sum_tot := sum_tot + v;
end;
  exit(sum_tot);
end;
function sumStr(xs: IntArray): string;
begin
  sumStr_s := '';
  sumStr_i := 0;
  while sumStr_i < Length(xs) do begin
  sumStr_s := (sumStr_s + IntToStr(xs[sumStr_i])) + ' + ';
  sumStr_i := sumStr_i + 1;
end;
  exit(copy(sumStr_s, 0+1, (Length(sumStr_s) - 3 - (0))));
end;
function pad2(n: integer): string;
begin
  pad2_s := IntToStr(n);
  if Length(pad2_s) < 2 then begin
  exit(' ' + pad2_s);
end;
  exit(pad2_s);
end;
function pad5(n: integer): string;
begin
  pad5_s := IntToStr(n);
  while Length(pad5_s) < 5 do begin
  pad5_s := ' ' + pad5_s;
end;
  exit(pad5_s);
end;
function abundantOdd(searchFrom: integer; countFrom: integer; countTo: integer; printOne: boolean): integer;
begin
  abundantOdd_count := countFrom;
  abundantOdd_n := searchFrom;
  while abundantOdd_count < countTo do begin
  abundantOdd_divs := divisors(abundantOdd_n);
  abundantOdd_tot := sum(abundantOdd_divs);
  if abundantOdd_tot > abundantOdd_n then begin
  abundantOdd_count := abundantOdd_count + 1;
  if printOne and (abundantOdd_count < countTo) then begin
  abundantOdd_n := abundantOdd_n + 2;
  continue;
end;
  abundantOdd_s := sumStr(abundantOdd_divs);
  if not printOne then begin
  writeln((((((pad2(abundantOdd_count) + '. ') + pad5(abundantOdd_n)) + ' < ') + abundantOdd_s) + ' = ') + IntToStr(abundantOdd_tot));
end else begin
  writeln((((IntToStr(abundantOdd_n) + ' < ') + abundantOdd_s) + ' = ') + IntToStr(abundantOdd_tot));
end;
end;
  abundantOdd_n := abundantOdd_n + 2;
end;
  exit(abundantOdd_n);
end;
procedure main();
begin
  main_max := 25;
  writeln(('The first ' + IntToStr(main_max)) + ' abundant odd numbers are:');
  main_n := abundantOdd(1, 0, main_max, false);
  writeln('' + #10 + 'The one thousandth abundant odd number is:');
  abundantOdd(main_n, main_max, 1000, true);
  writeln('' + #10 + 'The first abundant odd number above one billion is:');
  abundantOdd(1000000001, 0, 1, true);
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

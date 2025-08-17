{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
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
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: integer;
  b: integer;
  den: integer;
function gcd(a: integer; b: integer): integer; forward;
function proper_fractions(den: integer): StrArray; forward;
procedure test_proper_fractions(); forward;
procedure main(); forward;
function gcd(a: integer; b: integer): integer;
var
  gcd_x: integer;
  gcd_y: integer;
  gcd_t: integer;
begin
  gcd_x := a;
  gcd_y := b;
  while gcd_y <> 0 do begin
  gcd_t := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_t;
end;
  if gcd_x < 0 then begin
  exit(-gcd_x);
end;
  exit(gcd_x);
end;
function proper_fractions(den: integer): StrArray;
var
  proper_fractions_res: array of string;
  proper_fractions_n: integer;
begin
  if den < 0 then begin
  panic('The Denominator Cannot be less than 0');
end;
  proper_fractions_res := [];
  proper_fractions_n := 1;
  while proper_fractions_n < den do begin
  if gcd(proper_fractions_n, den) = 1 then begin
  proper_fractions_res := concat(proper_fractions_res, StrArray([(IntToStr(proper_fractions_n) + '/') + IntToStr(den)]));
end;
  proper_fractions_n := proper_fractions_n + 1;
end;
  exit(proper_fractions_res);
end;
procedure test_proper_fractions();
var
  test_proper_fractions_a: StrArray;
  test_proper_fractions_b: StrArray;
  test_proper_fractions_c: StrArray;
begin
  test_proper_fractions_a := proper_fractions(10);
  if list_to_str(test_proper_fractions_a) <> list_to_str(['1/10', '3/10', '7/10', '9/10']) then begin
  panic('test 10 failed');
end;
  test_proper_fractions_b := proper_fractions(5);
  if list_to_str(test_proper_fractions_b) <> list_to_str(['1/5', '2/5', '3/5', '4/5']) then begin
  panic('test 5 failed');
end;
  test_proper_fractions_c := proper_fractions(0);
  if list_to_str(test_proper_fractions_c) <> list_int_to_str([]) then begin
  panic('test 0 failed');
end;
end;
procedure main();
begin
  test_proper_fractions();
  writeln(list_to_str(proper_fractions(10)));
  writeln(list_to_str(proper_fractions(5)));
  writeln(list_to_str(proper_fractions(0)));
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

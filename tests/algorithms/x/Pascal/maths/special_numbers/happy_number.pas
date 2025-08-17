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
  num: integer;
function is_happy_number(num: integer): boolean; forward;
procedure test_is_happy_number(); forward;
procedure main(); forward;
function is_happy_number(num: integer): boolean;
var
  is_happy_number_seen: array of integer;
  is_happy_number_n: integer;
  is_happy_number_i: integer;
  is_happy_number_total: integer;
  is_happy_number_temp: integer;
  is_happy_number_digit: integer;
begin
  if num <= 0 then begin
  panic('num must be a positive integer');
end;
  is_happy_number_seen := [];
  is_happy_number_n := num;
  while is_happy_number_n <> 1 do begin
  is_happy_number_i := 0;
  while is_happy_number_i < Length(is_happy_number_seen) do begin
  if is_happy_number_seen[is_happy_number_i] = is_happy_number_n then begin
  exit(false);
end;
  is_happy_number_i := is_happy_number_i + 1;
end;
  is_happy_number_seen := concat(is_happy_number_seen, IntArray([is_happy_number_n]));
  is_happy_number_total := 0;
  is_happy_number_temp := is_happy_number_n;
  while is_happy_number_temp > 0 do begin
  is_happy_number_digit := is_happy_number_temp mod 10;
  is_happy_number_total := is_happy_number_total + (is_happy_number_digit * is_happy_number_digit);
  is_happy_number_temp := is_happy_number_temp div 10;
end;
  is_happy_number_n := is_happy_number_total;
end;
  exit(true);
end;
procedure test_is_happy_number();
begin
  if not is_happy_number(19) then begin
  panic('19 should be happy');
end;
  if is_happy_number(2) then begin
  panic('2 should be unhappy');
end;
  if not is_happy_number(23) then begin
  panic('23 should be happy');
end;
  if not is_happy_number(1) then begin
  panic('1 should be happy');
end;
end;
procedure main();
begin
  test_is_happy_number();
  writeln(Ord(is_happy_number(19)));
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

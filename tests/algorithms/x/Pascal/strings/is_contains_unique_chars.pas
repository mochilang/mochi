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
function ord_(ord__ch: string): integer; forward;
function lshift(lshift_num: integer; lshift_k: integer): integer; forward;
function rshift(rshift_num: integer; rshift_k: integer): integer; forward;
function is_contains_unique_chars(is_contains_unique_chars_input_str: string): boolean; forward;
function ord_(ord__ch: string): integer;
var
  ord__lower: string;
  ord__upper: string;
  ord__digits: string;
  ord__i: integer;
begin
  ord__lower := 'abcdefghijklmnopqrstuvwxyz';
  ord__upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  ord__digits := '0123456789';
  ord__i := 0;
  while ord__i < Length(ord__lower) do begin
  if ord__lower[ord__i+1] = ord__ch then begin
  exit(97 + ord__i);
end;
  ord__i := ord__i + 1;
end;
  ord__i := 0;
  while ord__i < Length(ord__upper) do begin
  if ord__upper[ord__i+1] = ord__ch then begin
  exit(65 + ord__i);
end;
  ord__i := ord__i + 1;
end;
  ord__i := 0;
  while ord__i < Length(ord__digits) do begin
  if ord__digits[ord__i+1] = ord__ch then begin
  exit(48 + ord__i);
end;
  ord__i := ord__i + 1;
end;
  if ord__ch = ' ' then begin
  exit(32);
end;
  if ord__ch = '_' then begin
  exit(95);
end;
  if ord__ch = '.' then begin
  exit(46);
end;
  if ord__ch = '''' then begin
  exit(39);
end;
  exit(0);
end;
function lshift(lshift_num: integer; lshift_k: integer): integer;
var
  lshift_result_: integer;
  lshift_i: integer;
begin
  lshift_result_ := lshift_num;
  lshift_i := 0;
  while lshift_i < lshift_k do begin
  lshift_result_ := lshift_result_ * 2;
  lshift_i := lshift_i + 1;
end;
  exit(lshift_result_);
end;
function rshift(rshift_num: integer; rshift_k: integer): integer;
var
  rshift_result_: integer;
  rshift_i: integer;
begin
  rshift_result_ := rshift_num;
  rshift_i := 0;
  while rshift_i < rshift_k do begin
  rshift_result_ := (rshift_result_ - (rshift_result_ mod 2)) div 2;
  rshift_i := rshift_i + 1;
end;
  exit(rshift_result_);
end;
function is_contains_unique_chars(is_contains_unique_chars_input_str: string): boolean;
var
  is_contains_unique_chars_bitmap: integer;
  is_contains_unique_chars_i: integer;
  is_contains_unique_chars_code: integer;
begin
  is_contains_unique_chars_bitmap := 0;
  is_contains_unique_chars_i := 0;
  while is_contains_unique_chars_i < Length(is_contains_unique_chars_input_str) do begin
  is_contains_unique_chars_code := ord_(is_contains_unique_chars_input_str[is_contains_unique_chars_i+1]);
  if (rshift(is_contains_unique_chars_bitmap, is_contains_unique_chars_code) mod 2) = 1 then begin
  exit(false);
end;
  is_contains_unique_chars_bitmap := is_contains_unique_chars_bitmap + lshift(1, is_contains_unique_chars_code);
  is_contains_unique_chars_i := is_contains_unique_chars_i + 1;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(is_contains_unique_chars('I_love.py'), true)));
  writeln(LowerCase(BoolToStr(is_contains_unique_chars('I don''t love Python'), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

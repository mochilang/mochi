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
  num: integer;
function decimal_to_negative_base_2(num: integer): integer; forward;
function decimal_to_negative_base_2(num: integer): integer;
var
  decimal_to_negative_base_2_n: integer;
  decimal_to_negative_base_2_ans: string;
  decimal_to_negative_base_2_rem: integer;
begin
  if num = 0 then begin
  exit(0);
end;
  decimal_to_negative_base_2_n := num;
  decimal_to_negative_base_2_ans := '';
  while decimal_to_negative_base_2_n <> 0 do begin
  decimal_to_negative_base_2_rem := decimal_to_negative_base_2_n mod -2;
  decimal_to_negative_base_2_n := decimal_to_negative_base_2_n div -2;
  if decimal_to_negative_base_2_rem < 0 then begin
  decimal_to_negative_base_2_rem := decimal_to_negative_base_2_rem + 2;
  decimal_to_negative_base_2_n := decimal_to_negative_base_2_n + 1;
end;
  decimal_to_negative_base_2_ans := IntToStr(decimal_to_negative_base_2_rem) + decimal_to_negative_base_2_ans;
end;
  exit(StrToInt(decimal_to_negative_base_2_ans));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(decimal_to_negative_base_2(0));
  writeln(decimal_to_negative_base_2(-19));
  writeln(decimal_to_negative_base_2(4));
  writeln(decimal_to_negative_base_2(7));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

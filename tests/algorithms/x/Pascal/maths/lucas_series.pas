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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  n: integer;
function recursive_lucas_number(n: integer): integer; forward;
function dynamic_lucas_number(n: integer): integer; forward;
function recursive_lucas_number(n: integer): integer;
begin
  if n = 0 then begin
  exit(2);
end;
  if n = 1 then begin
  exit(1);
end;
  exit(recursive_lucas_number(n - 1) + recursive_lucas_number(n - 2));
end;
function dynamic_lucas_number(n: integer): integer;
var
  dynamic_lucas_number_a: integer;
  dynamic_lucas_number_b: integer;
  dynamic_lucas_number_i: integer;
  dynamic_lucas_number_next: integer;
begin
  dynamic_lucas_number_a := 2;
  dynamic_lucas_number_b := 1;
  dynamic_lucas_number_i := 0;
  while dynamic_lucas_number_i < n do begin
  dynamic_lucas_number_next := dynamic_lucas_number_a + dynamic_lucas_number_b;
  dynamic_lucas_number_a := dynamic_lucas_number_b;
  dynamic_lucas_number_b := dynamic_lucas_number_next;
  dynamic_lucas_number_i := dynamic_lucas_number_i + 1;
end;
  exit(dynamic_lucas_number_a);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(recursive_lucas_number(1)));
  writeln(IntToStr(recursive_lucas_number(20)));
  writeln(IntToStr(recursive_lucas_number(0)));
  writeln(IntToStr(recursive_lucas_number(5)));
  writeln(IntToStr(dynamic_lucas_number(1)));
  writeln(IntToStr(dynamic_lucas_number(20)));
  writeln(IntToStr(dynamic_lucas_number(0)));
  writeln(IntToStr(dynamic_lucas_number(25)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

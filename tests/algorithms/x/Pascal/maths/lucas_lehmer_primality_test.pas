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
  p: integer;
function pow2(p: integer): integer; forward;
function lucas_lehmer_test(p: integer): boolean; forward;
procedure main(); forward;
function pow2(p: integer): integer;
var
  pow2_result_: integer;
  pow2_i: integer;
begin
  pow2_result_ := 1;
  pow2_i := 0;
  while pow2_i < p do begin
  pow2_result_ := pow2_result_ * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_result_);
end;
function lucas_lehmer_test(p: integer): boolean;
var
  lucas_lehmer_test_s: integer;
  lucas_lehmer_test_m: integer;
  lucas_lehmer_test_i: integer;
begin
  if p < 2 then begin
  panic('p should not be less than 2!');
end;
  if p = 2 then begin
  exit(true);
end;
  lucas_lehmer_test_s := 4;
  lucas_lehmer_test_m := pow2(p) - 1;
  lucas_lehmer_test_i := 0;
  while lucas_lehmer_test_i < (p - 2) do begin
  lucas_lehmer_test_s := ((lucas_lehmer_test_s * lucas_lehmer_test_s) - 2) mod lucas_lehmer_test_m;
  lucas_lehmer_test_i := lucas_lehmer_test_i + 1;
end;
  exit(lucas_lehmer_test_s = 0);
end;
procedure main();
begin
  writeln(LowerCase(BoolToStr(lucas_lehmer_test(7), true)));
  writeln(LowerCase(BoolToStr(lucas_lehmer_test(11), true)));
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

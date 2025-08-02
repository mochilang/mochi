{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function powf(base: real; exp: integer): real; forward;
function nthRoot(x: real; n: integer): real; forward;
procedure main(); forward;
function powf(base: real; exp: integer): real;
var
  powf_result: real;
  powf_i: integer;
begin
  powf_result := 1;
  powf_i := 0;
  while powf_i < exp do begin
  powf_result := powf_result * base;
  powf_i := powf_i + 1;
end;
  exit(powf_result);
end;
function nthRoot(x: real; n: integer): real;
var
  nthRoot_low: real;
  nthRoot_high: real;
  nthRoot_i: integer;
  nthRoot_mid: real;
begin
  nthRoot_low := 0;
  nthRoot_high := x;
  nthRoot_i := 0;
  while nthRoot_i < 60 do begin
  nthRoot_mid := (nthRoot_low + nthRoot_high) / 2;
  if powf(nthRoot_mid, n) > x then begin
  nthRoot_high := nthRoot_mid;
end else begin
  nthRoot_low := nthRoot_mid;
end;
  nthRoot_i := nthRoot_i + 1;
end;
  exit(nthRoot_low);
end;
procedure main();
var
  main_sum: real;
  main_sumRecip: real;
  main_prod: real;
  main_n: integer;
  main_f: real;
  main_count: real;
  main_a: real;
  main_g: real;
  main_h: real;
begin
  main_sum := 0;
  main_sumRecip := 0;
  main_prod := 1;
  main_n := 1;
  while main_n <= 10 do begin
  main_f := Double(main_n);
  main_sum := main_sum + main_f;
  main_sumRecip := main_sumRecip + (1 / main_f);
  main_prod := main_prod * main_f;
  main_n := main_n + 1;
end;
  main_count := 10;
  main_a := main_sum / main_count;
  main_g := nthRoot(main_prod, 10);
  main_h := main_count / main_sumRecip;
  writeln((((('A: ' + FloatToStr(main_a)) + ' G: ') + FloatToStr(main_g)) + ' H: ') + FloatToStr(main_h));
  writeln('A >= G >= H: ' + LowerCase(BoolToStr((main_a >= main_g) and (main_g >= main_h), true)));
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

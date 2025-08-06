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
function largest_pow_of_two_le_num(n: integer): integer; forward;
function largest_pow_of_two_le_num(n: integer): integer;
var
  largest_pow_of_two_le_num_res: integer;
begin
  if n <= 0 then begin
  exit(0);
end;
  largest_pow_of_two_le_num_res := 1;
  while (largest_pow_of_two_le_num_res * 2) <= n do begin
  largest_pow_of_two_le_num_res := largest_pow_of_two_le_num_res * 2;
end;
  exit(largest_pow_of_two_le_num_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(largest_pow_of_two_le_num(0)));
  writeln(IntToStr(largest_pow_of_two_le_num(1)));
  writeln(IntToStr(largest_pow_of_two_le_num(-1)));
  writeln(IntToStr(largest_pow_of_two_le_num(3)));
  writeln(IntToStr(largest_pow_of_two_le_num(15)));
  writeln(IntToStr(largest_pow_of_two_le_num(99)));
  writeln(IntToStr(largest_pow_of_two_le_num(178)));
  writeln(IntToStr(largest_pow_of_two_le_num(999999)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

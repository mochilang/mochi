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
  countChange_ways: array of integer;
  countChange_i: integer;
  coin: integer;
  countChange_j: integer;
  amount: integer;
function countChange(amount: integer): integer; forward;
function countChange(amount: integer): integer;
begin
  countChange_ways := [];
  countChange_i := 0;
  while countChange_i <= amount do begin
  countChange_ways := concat(countChange_ways, [0]);
  countChange_i := countChange_i + 1;
end;
  countChange_ways[0] := 1;
  for coin in [100, 50, 25, 10, 5, 1] do begin
  countChange_j := coin;
  while countChange_j <= amount do begin
  countChange_ways[countChange_j] := countChange_ways[countChange_j] + countChange_ways[countChange_j - coin];
  countChange_j := countChange_j + 1;
end;
end;
  exit(countChange_ways[amount]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  amount := 1000;
  writeln((('amount, ways to make change: ' + IntToStr(amount)) + ' ') + IntToStr(countChange(amount)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

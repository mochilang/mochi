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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function randOrder(seed: integer; n: integer): IntArray; forward;
function randChaos(seed: integer; n: integer): IntArray; forward;
procedure main(); forward;
function randOrder(seed: integer; n: integer): IntArray;
var
  randOrder_next: integer;
begin
  randOrder_next := ((seed * 1664525) + 1013904223) mod 2147483647;
  exit([randOrder_next, randOrder_next mod n]);
end;
function randChaos(seed: integer; n: integer): IntArray;
var
  randChaos_next: integer;
begin
  randChaos_next := ((seed * 1103515245) + 12345) mod 2147483647;
  exit([randChaos_next, randChaos_next mod n]);
end;
procedure main();
var
  main_nBuckets: integer;
  main_initialSum: integer;
  main_buckets: array of integer;
  main_i: integer;
  main_dist: integer;
  main_v: integer;
  main_tc0: integer;
  main_tc1: integer;
  main_total: integer;
  main_nTicks: integer;
  main_seedOrder: integer;
  main_seedChaos: integer;
  main_t: integer;
  main_r: IntArray;
  main_b1: integer;
  main_b2: integer;
  main_v1: integer;
  main_v2: integer;
  main_a: integer;
  main_amt: integer;
  main_sum: integer;
  main_idx: integer;
begin
  main_nBuckets := 10;
  main_initialSum := 1000;
  main_buckets := [];
  for main_i := 0 to (main_nBuckets - 1) do begin
  main_buckets := concat(main_buckets, [0]);
end;
  main_i := main_nBuckets;
  main_dist := main_initialSum;
  while main_i > 0 do begin
  main_v := main_dist div main_i;
  main_i := main_i - 1;
  main_buckets[main_i] := main_v;
  main_dist := main_dist - main_v;
end;
  main_tc0 := 0;
  main_tc1 := 0;
  main_total := 0;
  main_nTicks := 0;
  main_seedOrder := 1;
  main_seedChaos := 2;
  writeln('sum  ---updates---    mean  buckets');
  main_t := 0;
  while main_t < 5 do begin
  main_r := randOrder(main_seedOrder, main_nBuckets);
  main_seedOrder := main_r[0];
  main_b1 := main_r[1];
  main_b2 := (main_b1 + 1) mod main_nBuckets;
  main_v1 := main_buckets[main_b1];
  main_v2 := main_buckets[main_b2];
  if main_v1 > main_v2 then begin
  main_a := Trunc((main_v1 - main_v2) div 2);
  if main_a > main_buckets[main_b1] then begin
  main_a := main_buckets[main_b1];
end;
  main_buckets[main_b1] := main_buckets[main_b1] - main_a;
  main_buckets[main_b2] := main_buckets[main_b2] + main_a;
end else begin
  main_a := Trunc((main_v2 - main_v1) div 2);
  if main_a > main_buckets[main_b2] then begin
  main_a := main_buckets[main_b2];
end;
  main_buckets[main_b2] := main_buckets[main_b2] - main_a;
  main_buckets[main_b1] := main_buckets[main_b1] + main_a;
end;
  main_tc0 := main_tc0 + 1;
  main_r := randChaos(main_seedChaos, main_nBuckets);
  main_seedChaos := main_r[0];
  main_b1 := main_r[1];
  main_b2 := (main_b1 + 1) mod main_nBuckets;
  main_r := randChaos(main_seedChaos, main_buckets[main_b1] + 1);
  main_seedChaos := main_r[0];
  main_amt := main_r[1];
  if main_amt > main_buckets[main_b1] then begin
  main_amt := main_buckets[main_b1];
end;
  main_buckets[main_b1] := main_buckets[main_b1] - main_amt;
  main_buckets[main_b2] := main_buckets[main_b2] + main_amt;
  main_tc1 := main_tc1 + 1;
  main_sum := 0;
  main_idx := 0;
  while main_idx < main_nBuckets do begin
  main_sum := main_sum + main_buckets[main_idx];
  main_idx := main_idx + 1;
end;
  main_total := (main_total + main_tc0) + main_tc1;
  main_nTicks := main_nTicks + 1;
  writeln((((((((IntToStr(main_sum) + ' ') + IntToStr(main_tc0)) + ' ') + IntToStr(main_tc1)) + ' ') + IntToStr(main_total div main_nTicks)) + '  ') + list_int_to_str(main_buckets));
  main_tc0 := 0;
  main_tc1 := 0;
  main_t := main_t + 1;
end;
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

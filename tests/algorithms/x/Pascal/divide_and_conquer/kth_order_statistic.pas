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
  k: integer;
  lst: IntArray;
function pivot(lst: IntArray): integer; forward;
function kth_number(lst: IntArray; k: integer): integer; forward;
function pivot(lst: IntArray): integer;
begin
  exit(lst[0]);
end;
function kth_number(lst: IntArray; k: integer): integer;
var
  kth_number_p: integer;
  kth_number_small: array of integer;
  kth_number_big: array of integer;
  kth_number_i: integer;
  kth_number_e: integer;
begin
  kth_number_p := pivot(lst);
  kth_number_small := [];
  kth_number_big := [];
  kth_number_i := 0;
  while kth_number_i < Length(lst) do begin
  kth_number_e := lst[kth_number_i];
  if kth_number_e < kth_number_p then begin
  kth_number_small := concat(kth_number_small, IntArray([kth_number_e]));
end else begin
  if kth_number_e > kth_number_p then begin
  kth_number_big := concat(kth_number_big, IntArray([kth_number_e]));
end;
end;
  kth_number_i := kth_number_i + 1;
end;
  if Length(kth_number_small) = (k - 1) then begin
  exit(kth_number_p);
end else begin
  if Length(kth_number_small) < (k - 1) then begin
  exit(kth_number(kth_number_big, (k - Length(kth_number_small)) - 1));
end else begin
  exit(kth_number(kth_number_small, k));
end;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(kth_number([2, 1, 3, 4, 5], 3)));
  writeln(IntToStr(kth_number([2, 1, 3, 4, 5], 1)));
  writeln(IntToStr(kth_number([2, 1, 3, 4, 5], 5)));
  writeln(IntToStr(kth_number([3, 2, 5, 6, 7, 8], 2)));
  writeln(IntToStr(kth_number([25, 21, 98, 100, 76, 22, 43, 60, 89, 87], 4)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

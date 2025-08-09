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
  prices: array of integer;
  n: integer;
procedure enforce_args(n: integer; prices: IntArray); forward;
function bottom_up_cut_rod(n: integer; prices: IntArray): integer; forward;
procedure enforce_args(n: integer; prices: IntArray);
begin
  if n < 0 then begin
  panic('n must be non-negative');
end;
  if n > Length(prices) then begin
  panic('price list is shorter than n');
end;
end;
function bottom_up_cut_rod(n: integer; prices: IntArray): integer;
var
  bottom_up_cut_rod_max_rev: array of integer;
  bottom_up_cut_rod_i: integer;
  bottom_up_cut_rod_length_: integer;
  bottom_up_cut_rod_best: integer;
  bottom_up_cut_rod_j: integer;
  bottom_up_cut_rod_candidate: integer;
begin
  enforce_args(n, prices);
  bottom_up_cut_rod_i := 0;
  while bottom_up_cut_rod_i <= n do begin
  if bottom_up_cut_rod_i = 0 then begin
  bottom_up_cut_rod_max_rev := concat(bottom_up_cut_rod_max_rev, IntArray([0]));
end else begin
  bottom_up_cut_rod_max_rev := concat(bottom_up_cut_rod_max_rev, IntArray([-2147483648]));
end;
  bottom_up_cut_rod_i := bottom_up_cut_rod_i + 1;
end;
  bottom_up_cut_rod_length_ := 1;
  while bottom_up_cut_rod_length_ <= n do begin
  bottom_up_cut_rod_best := bottom_up_cut_rod_max_rev[bottom_up_cut_rod_length_];
  bottom_up_cut_rod_j := 1;
  while bottom_up_cut_rod_j <= bottom_up_cut_rod_length_ do begin
  bottom_up_cut_rod_candidate := prices[bottom_up_cut_rod_j - 1] + bottom_up_cut_rod_max_rev[bottom_up_cut_rod_length_ - bottom_up_cut_rod_j];
  if bottom_up_cut_rod_candidate > bottom_up_cut_rod_best then begin
  bottom_up_cut_rod_best := bottom_up_cut_rod_candidate;
end;
  bottom_up_cut_rod_j := bottom_up_cut_rod_j + 1;
end;
  bottom_up_cut_rod_max_rev[bottom_up_cut_rod_length_] := bottom_up_cut_rod_best;
  bottom_up_cut_rod_length_ := bottom_up_cut_rod_length_ + 1;
end;
  exit(bottom_up_cut_rod_max_rev[n]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  prices := [1, 5, 8, 9, 10, 17, 17, 20, 24, 30];
  writeln(bottom_up_cut_rod(4, prices));
  writeln(bottom_up_cut_rod(10, prices));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

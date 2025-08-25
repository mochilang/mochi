{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  prices: array of int64;
procedure enforce_args(enforce_args_n: int64; enforce_args_prices: IntArray); forward;
function bottom_up_cut_rod(bottom_up_cut_rod_n: int64; bottom_up_cut_rod_prices: IntArray): int64; forward;
procedure enforce_args(enforce_args_n: int64; enforce_args_prices: IntArray);
begin
  if enforce_args_n < 0 then begin
  panic('n must be non-negative');
end;
  if enforce_args_n > Length(enforce_args_prices) then begin
  panic('price list is shorter than n');
end;
end;
function bottom_up_cut_rod(bottom_up_cut_rod_n: int64; bottom_up_cut_rod_prices: IntArray): int64;
var
  bottom_up_cut_rod_max_rev: array of int64;
  bottom_up_cut_rod_i: int64;
  bottom_up_cut_rod_length_: int64;
  bottom_up_cut_rod_best: int64;
  bottom_up_cut_rod_j: int64;
  bottom_up_cut_rod_candidate: int64;
begin
  enforce_args(bottom_up_cut_rod_n, bottom_up_cut_rod_prices);
  bottom_up_cut_rod_i := 0;
  while bottom_up_cut_rod_i <= bottom_up_cut_rod_n do begin
  if bottom_up_cut_rod_i = 0 then begin
  bottom_up_cut_rod_max_rev := concat(bottom_up_cut_rod_max_rev, IntArray([0]));
end else begin
  bottom_up_cut_rod_max_rev := concat(bottom_up_cut_rod_max_rev, IntArray([-2147483648]));
end;
  bottom_up_cut_rod_i := bottom_up_cut_rod_i + 1;
end;
  bottom_up_cut_rod_length_ := 1;
  while bottom_up_cut_rod_length_ <= bottom_up_cut_rod_n do begin
  bottom_up_cut_rod_best := bottom_up_cut_rod_max_rev[bottom_up_cut_rod_length_];
  bottom_up_cut_rod_j := 1;
  while bottom_up_cut_rod_j <= bottom_up_cut_rod_length_ do begin
  bottom_up_cut_rod_candidate := bottom_up_cut_rod_prices[bottom_up_cut_rod_j - 1] + bottom_up_cut_rod_max_rev[bottom_up_cut_rod_length_ - bottom_up_cut_rod_j];
  if bottom_up_cut_rod_candidate > bottom_up_cut_rod_best then begin
  bottom_up_cut_rod_best := bottom_up_cut_rod_candidate;
end;
  bottom_up_cut_rod_j := bottom_up_cut_rod_j + 1;
end;
  bottom_up_cut_rod_max_rev[bottom_up_cut_rod_length_] := bottom_up_cut_rod_best;
  bottom_up_cut_rod_length_ := bottom_up_cut_rod_length_ + 1;
end;
  exit(bottom_up_cut_rod_max_rev[bottom_up_cut_rod_n]);
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
  writeln('');
end.

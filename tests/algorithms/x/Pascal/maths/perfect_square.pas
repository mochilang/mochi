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
  n: integer;
  num: integer;
function perfect_square(num: integer): boolean; forward;
function perfect_square_binary_search(n: integer): boolean; forward;
function perfect_square(num: integer): boolean;
var
  perfect_square_i: integer;
begin
  if num < 0 then begin
  exit(false);
end;
  perfect_square_i := 0;
  while (perfect_square_i * perfect_square_i) <= num do begin
  if (perfect_square_i * perfect_square_i) = num then begin
  exit(true);
end;
  perfect_square_i := perfect_square_i + 1;
end;
  exit(false);
end;
function perfect_square_binary_search(n: integer): boolean;
var
  perfect_square_binary_search_left: integer;
  perfect_square_binary_search_right: integer;
  perfect_square_binary_search_mid: integer;
  perfect_square_binary_search_sq: integer;
begin
  if n < 0 then begin
  exit(false);
end;
  perfect_square_binary_search_left := 0;
  perfect_square_binary_search_right := n;
  while perfect_square_binary_search_left <= perfect_square_binary_search_right do begin
  perfect_square_binary_search_mid := (perfect_square_binary_search_left + perfect_square_binary_search_right) div 2;
  perfect_square_binary_search_sq := perfect_square_binary_search_mid * perfect_square_binary_search_mid;
  if perfect_square_binary_search_sq = n then begin
  exit(true);
end;
  if perfect_square_binary_search_sq > n then begin
  perfect_square_binary_search_right := perfect_square_binary_search_mid - 1;
end else begin
  perfect_square_binary_search_left := perfect_square_binary_search_mid + 1;
end;
end;
  exit(false);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(perfect_square(9), true)));
  writeln(LowerCase(BoolToStr(perfect_square(10), true)));
  writeln(LowerCase(BoolToStr(perfect_square_binary_search(16), true)));
  writeln(LowerCase(BoolToStr(perfect_square_binary_search(10), true)));
  writeln(LowerCase(BoolToStr(perfect_square_binary_search(-1), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  upper_limit: integer;
  msg: string;
procedure panic_(msg: string); forward;
function catalan_numbers(upper_limit: integer): IntArray; forward;
procedure panic_(msg: string);
begin
  writeln(msg);
end;
function catalan_numbers(upper_limit: integer): IntArray;
var
  catalan_numbers_catalans: array of integer;
  catalan_numbers_n: integer;
  catalan_numbers_next_val: integer;
  catalan_numbers_j: integer;
begin
  if upper_limit < 0 then begin
  panic_('Limit for the Catalan sequence must be >= 0');
  exit([]);
end;
  catalan_numbers_catalans := [1];
  catalan_numbers_n := 1;
  while catalan_numbers_n <= upper_limit do begin
  catalan_numbers_next_val := 0;
  catalan_numbers_j := 0;
  while catalan_numbers_j < catalan_numbers_n do begin
  catalan_numbers_next_val := catalan_numbers_next_val + (catalan_numbers_catalans[catalan_numbers_j] * catalan_numbers_catalans[(catalan_numbers_n - catalan_numbers_j) - 1]);
  catalan_numbers_j := catalan_numbers_j + 1;
end;
  catalan_numbers_catalans := concat(catalan_numbers_catalans, IntArray([catalan_numbers_next_val]));
  catalan_numbers_n := catalan_numbers_n + 1;
end;
  exit(catalan_numbers_catalans);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(catalan_numbers(5)));
  writeln(list_int_to_str(catalan_numbers(2)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

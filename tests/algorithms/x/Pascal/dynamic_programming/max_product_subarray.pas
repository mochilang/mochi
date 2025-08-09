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
  numbers: IntArray;
function max_product_subarray(numbers: IntArray): integer; forward;
function max_product_subarray(numbers: IntArray): integer;
var
  max_product_subarray_max_till_now: integer;
  max_product_subarray_min_till_now: integer;
  max_product_subarray_max_prod: integer;
  max_product_subarray_i: integer;
  max_product_subarray_number: integer;
  max_product_subarray_temp: integer;
  max_product_subarray_prod_max: integer;
  max_product_subarray_prod_min: integer;
begin
  if Length(numbers) = 0 then begin
  exit(0);
end;
  max_product_subarray_max_till_now := numbers[0];
  max_product_subarray_min_till_now := numbers[0];
  max_product_subarray_max_prod := numbers[0];
  max_product_subarray_i := 1;
  while max_product_subarray_i < Length(numbers) do begin
  max_product_subarray_number := numbers[max_product_subarray_i];
  if max_product_subarray_number < 0 then begin
  max_product_subarray_temp := max_product_subarray_max_till_now;
  max_product_subarray_max_till_now := max_product_subarray_min_till_now;
  max_product_subarray_min_till_now := max_product_subarray_temp;
end;
  max_product_subarray_prod_max := max_product_subarray_max_till_now * max_product_subarray_number;
  if max_product_subarray_number > max_product_subarray_prod_max then begin
  max_product_subarray_max_till_now := max_product_subarray_number;
end else begin
  max_product_subarray_max_till_now := max_product_subarray_prod_max;
end;
  max_product_subarray_prod_min := max_product_subarray_min_till_now * max_product_subarray_number;
  if max_product_subarray_number < max_product_subarray_prod_min then begin
  max_product_subarray_min_till_now := max_product_subarray_number;
end else begin
  max_product_subarray_min_till_now := max_product_subarray_prod_min;
end;
  if max_product_subarray_max_till_now > max_product_subarray_max_prod then begin
  max_product_subarray_max_prod := max_product_subarray_max_till_now;
end;
  max_product_subarray_i := max_product_subarray_i + 1;
end;
  exit(max_product_subarray_max_prod);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(max_product_subarray([2, 3, -2, 4]));
  writeln(max_product_subarray([-2, 0, -1]));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

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
  mask: integer;
  a: integer;
  b: integer;
function bitwise_and(a: integer; b: integer): integer; forward;
function list_of_submasks(mask: integer): IntArray; forward;
function bitwise_and(a: integer; b: integer): integer;
var
  bitwise_and_result_: integer;
  bitwise_and_bit: integer;
  bitwise_and_x: integer;
  bitwise_and_y: integer;
  bitwise_and_abit: integer;
  bitwise_and_bbit: integer;
begin
  bitwise_and_result_ := 0;
  bitwise_and_bit := 1;
  bitwise_and_x := a;
  bitwise_and_y := b;
  while (bitwise_and_x > 0) or (bitwise_and_y > 0) do begin
  bitwise_and_abit := bitwise_and_x mod 2;
  bitwise_and_bbit := bitwise_and_y mod 2;
  if (bitwise_and_abit = 1) and (bitwise_and_bbit = 1) then begin
  bitwise_and_result_ := bitwise_and_result_ + bitwise_and_bit;
end;
  bitwise_and_x := bitwise_and_x div 2;
  bitwise_and_y := bitwise_and_y div 2;
  bitwise_and_bit := bitwise_and_bit * 2;
end;
  exit(bitwise_and_result_);
end;
function list_of_submasks(mask: integer): IntArray;
var
  list_of_submasks_all_submasks: array of integer;
  list_of_submasks_submask: integer;
begin
  if mask <= 0 then begin
  panic('mask needs to be positive integer, your input ' + IntToStr(mask));
end;
  list_of_submasks_all_submasks := [];
  list_of_submasks_submask := mask;
  while list_of_submasks_submask <> 0 do begin
  list_of_submasks_all_submasks := concat(list_of_submasks_all_submasks, IntArray([list_of_submasks_submask]));
  list_of_submasks_submask := bitwise_and(list_of_submasks_submask - 1, mask);
end;
  exit(list_of_submasks_all_submasks);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(list_of_submasks(15)));
  writeln(list_int_to_str(list_of_submasks(13)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

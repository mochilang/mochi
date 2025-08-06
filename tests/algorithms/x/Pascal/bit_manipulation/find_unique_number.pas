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
  a: integer;
  b: integer;
  arr: IntArray;
function bit_xor(a: integer; b: integer): integer; forward;
function find_unique_number(arr: IntArray): integer; forward;
function bit_xor(a: integer; b: integer): integer;
var
  bit_xor_ua: integer;
  bit_xor_ub: integer;
  bit_xor_res: integer;
  bit_xor_bit: integer;
  bit_xor_abit: integer;
  bit_xor_bbit: integer;
begin
  bit_xor_ua := a;
  bit_xor_ub := b;
  bit_xor_res := 0;
  bit_xor_bit := 1;
  while (bit_xor_ua > 0) or (bit_xor_ub > 0) do begin
  bit_xor_abit := bit_xor_ua mod 2;
  bit_xor_bbit := bit_xor_ub mod 2;
  if ((bit_xor_abit = 1) and (bit_xor_bbit = 0)) or ((bit_xor_abit = 0) and (bit_xor_bbit = 1)) then begin
  bit_xor_res := bit_xor_res + bit_xor_bit;
end;
  bit_xor_ua := Trunc(bit_xor_ua div 2);
  bit_xor_ub := Trunc(bit_xor_ub div 2);
  bit_xor_bit := bit_xor_bit * 2;
end;
  exit(bit_xor_res);
end;
function find_unique_number(arr: IntArray): integer;
var
  find_unique_number_result_: integer;
  find_unique_number_num: integer;
begin
  if Length(arr) = 0 then begin
  panic('input list must not be empty');
end;
  find_unique_number_result_ := 0;
  for find_unique_number_num in arr do begin
  find_unique_number_result_ := bit_xor(find_unique_number_result_, find_unique_number_num);
end;
  exit(find_unique_number_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(find_unique_number([1, 1, 2, 2, 3])));
  writeln(IntToStr(find_unique_number([4, 5, 4, 6, 6])));
  writeln(IntToStr(find_unique_number([7])));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

{$mode objfpc}
program Main;
uses SysUtils, Math;
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
  n: integer;
  width: integer;
  a: integer;
  b: integer;
  s: string;
function int_to_binary(n: integer): string; forward;
function pad_left(s: string; width: integer): string; forward;
function binary_xor(a: integer; b: integer): string; forward;
function int_to_binary(n: integer): string;
var
  int_to_binary_res: string;
  int_to_binary_num: integer;
begin
  if n = 0 then begin
  exit('0');
end;
  int_to_binary_res := '';
  int_to_binary_num := n;
  while int_to_binary_num > 0 do begin
  int_to_binary_res := IntToStr(int_to_binary_num mod 2) + int_to_binary_res;
  int_to_binary_num := int_to_binary_num div 2;
end;
  exit(int_to_binary_res);
end;
function pad_left(s: string; width: integer): string;
var
  pad_left_res: string;
begin
  pad_left_res := s;
  while Length(pad_left_res) < width do begin
  pad_left_res := '0' + pad_left_res;
end;
  exit(pad_left_res);
end;
function binary_xor(a: integer; b: integer): string;
var
  binary_xor_a_bin: string;
  binary_xor_b_bin: string;
  binary_xor_max_len: integer;
  binary_xor_a_pad: string;
  binary_xor_b_pad: string;
  binary_xor_i: integer;
  binary_xor_result_: string;
begin
  if (a < 0) or (b < 0) then begin
  panic('the value of both inputs must be positive');
end;
  binary_xor_a_bin := int_to_binary(a);
  binary_xor_b_bin := int_to_binary(b);
  binary_xor_max_len := IfThen(Length(binary_xor_a_bin) > Length(binary_xor_b_bin), Length(binary_xor_a_bin), Length(binary_xor_b_bin));
  binary_xor_a_pad := pad_left(binary_xor_a_bin, binary_xor_max_len);
  binary_xor_b_pad := pad_left(binary_xor_b_bin, binary_xor_max_len);
  binary_xor_i := 0;
  binary_xor_result_ := '';
  while binary_xor_i < binary_xor_max_len do begin
  if binary_xor_a_pad[binary_xor_i+1] <> binary_xor_b_pad[binary_xor_i+1] then begin
  binary_xor_result_ := binary_xor_result_ + '1';
end else begin
  binary_xor_result_ := binary_xor_result_ + '0';
end;
  binary_xor_i := binary_xor_i + 1;
end;
  exit('0b' + binary_xor_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(binary_xor(25, 32));
  writeln(binary_xor(37, 50));
  writeln(binary_xor(21, 30));
  writeln(binary_xor(58, 73));
  writeln(binary_xor(0, 255));
  writeln(binary_xor(256, 256));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

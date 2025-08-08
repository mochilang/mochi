{$mode objfpc}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  values: array of string;
  decimal: integer;
function decimal_to_hexadecimal(decimal: integer): string; forward;
function decimal_to_hexadecimal(decimal: integer): string;
var
  decimal_to_hexadecimal_num: integer;
  decimal_to_hexadecimal_negative: boolean;
  decimal_to_hexadecimal_hex: string;
  decimal_to_hexadecimal_remainder: integer;
begin
  decimal_to_hexadecimal_num := decimal;
  decimal_to_hexadecimal_negative := false;
  if decimal_to_hexadecimal_num < 0 then begin
  decimal_to_hexadecimal_negative := true;
  decimal_to_hexadecimal_num := -decimal_to_hexadecimal_num;
end;
  if decimal_to_hexadecimal_num = 0 then begin
  if decimal_to_hexadecimal_negative then begin
  exit('-0x0');
end;
  exit('0x0');
end;
  decimal_to_hexadecimal_hex := '';
  while decimal_to_hexadecimal_num > 0 do begin
  decimal_to_hexadecimal_remainder := decimal_to_hexadecimal_num mod 16;
  decimal_to_hexadecimal_hex := values[decimal_to_hexadecimal_remainder] + decimal_to_hexadecimal_hex;
  decimal_to_hexadecimal_num := decimal_to_hexadecimal_num div 16;
end;
  if decimal_to_hexadecimal_negative then begin
  exit('-0x' + decimal_to_hexadecimal_hex);
end;
  exit('0x' + decimal_to_hexadecimal_hex);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  values := ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'];
  writeln(decimal_to_hexadecimal(5));
  writeln(decimal_to_hexadecimal(15));
  writeln(decimal_to_hexadecimal(37));
  writeln(decimal_to_hexadecimal(255));
  writeln(decimal_to_hexadecimal(4096));
  writeln(decimal_to_hexadecimal(999098));
  writeln(decimal_to_hexadecimal(-256));
  writeln(decimal_to_hexadecimal(0));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

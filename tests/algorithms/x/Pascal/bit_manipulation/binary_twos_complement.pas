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
  ch: string;
  number: integer;
  times: integer;
  n: integer;
  exp: integer;
function repeat_char(ch: string; times: integer): string; forward;
function to_binary(n: integer): string; forward;
function pow2(exp: integer): integer; forward;
function twos_complement(number: integer): string; forward;
function repeat_char(ch: string; times: integer): string;
var
  repeat_char_res: string;
  repeat_char_i: integer;
begin
  repeat_char_res := '';
  repeat_char_i := 0;
  while repeat_char_i < times do begin
  repeat_char_res := repeat_char_res + ch;
  repeat_char_i := repeat_char_i + 1;
end;
  exit(repeat_char_res);
end;
function to_binary(n: integer): string;
var
  to_binary_res: string;
  to_binary_v: integer;
begin
  if n = 0 then begin
  exit('0');
end;
  to_binary_res := '';
  to_binary_v := n;
  while to_binary_v > 0 do begin
  to_binary_res := IntToStr(to_binary_v mod 2) + to_binary_res;
  to_binary_v := to_binary_v div 2;
end;
  exit(to_binary_res);
end;
function pow2(exp: integer): integer;
var
  pow2_res: integer;
  pow2_i: integer;
begin
  pow2_res := 1;
  pow2_i := 0;
  while pow2_i < exp do begin
  pow2_res := pow2_res * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_res);
end;
function twos_complement(number: integer): string;
var
  twos_complement_abs_number: integer;
  twos_complement_binary_number_length: integer;
  twos_complement_complement_value: integer;
  twos_complement_complement_binary: string;
  twos_complement_padding: string;
  twos_complement_twos_complement_number: string;
begin
  if number > 0 then begin
  panic('input must be a negative integer');
end;
  if number = 0 then begin
  exit('0b0');
end;
  twos_complement_abs_number := IfThen(number < 0, -number, number);
  twos_complement_binary_number_length := Length(to_binary(twos_complement_abs_number));
  twos_complement_complement_value := pow2(twos_complement_binary_number_length) - twos_complement_abs_number;
  twos_complement_complement_binary := to_binary(twos_complement_complement_value);
  twos_complement_padding := repeat_char('0', twos_complement_binary_number_length - Length(twos_complement_complement_binary));
  twos_complement_twos_complement_number := ('1' + twos_complement_padding) + twos_complement_complement_binary;
  exit('0b' + twos_complement_twos_complement_number);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(twos_complement(0));
  writeln(twos_complement(-1));
  writeln(twos_complement(-5));
  writeln(twos_complement(-17));
  writeln(twos_complement(-207));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

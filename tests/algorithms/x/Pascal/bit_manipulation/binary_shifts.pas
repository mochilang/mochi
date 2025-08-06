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
  shift_amount: integer;
  ch: string;
  number: integer;
  count: integer;
  n: integer;
  exp: integer;
function repeat_char(ch: string; count: integer): string; forward;
function abs_int(n: integer): integer; forward;
function pow2(exp: integer): integer; forward;
function to_binary_no_prefix(n: integer): string; forward;
function logical_left_shift(number: integer; shift_amount: integer): string; forward;
function logical_right_shift(number: integer; shift_amount: integer): string; forward;
function arithmetic_right_shift(number: integer; shift_amount: integer): string; forward;
procedure main(); forward;
function repeat_char(ch: string; count: integer): string;
var
  repeat_char_res: string;
  repeat_char_i: integer;
begin
  repeat_char_res := '';
  repeat_char_i := 0;
  while repeat_char_i < count do begin
  repeat_char_res := repeat_char_res + ch;
  repeat_char_i := repeat_char_i + 1;
end;
  exit(repeat_char_res);
end;
function abs_int(n: integer): integer;
begin
  if n < 0 then begin
  exit(-n);
end;
  exit(n);
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
function to_binary_no_prefix(n: integer): string;
var
  to_binary_no_prefix_v: integer;
  to_binary_no_prefix_res: string;
begin
  to_binary_no_prefix_v := n;
  if to_binary_no_prefix_v < 0 then begin
  to_binary_no_prefix_v := -to_binary_no_prefix_v;
end;
  if to_binary_no_prefix_v = 0 then begin
  exit('0');
end;
  to_binary_no_prefix_res := '';
  while to_binary_no_prefix_v > 0 do begin
  to_binary_no_prefix_res := IntToStr(to_binary_no_prefix_v mod 2) + to_binary_no_prefix_res;
  to_binary_no_prefix_v := to_binary_no_prefix_v div 2;
end;
  exit(to_binary_no_prefix_res);
end;
function logical_left_shift(number: integer; shift_amount: integer): string;
var
  logical_left_shift_binary_number: string;
begin
  if (number < 0) or (shift_amount < 0) then begin
  panic('both inputs must be positive integers');
end;
  logical_left_shift_binary_number := '0b' + to_binary_no_prefix(number);
  exit(logical_left_shift_binary_number + repeat_char('0', shift_amount));
end;
function logical_right_shift(number: integer; shift_amount: integer): string;
var
  logical_right_shift_binary_number: string;
  logical_right_shift_shifted: string;
begin
  if (number < 0) or (shift_amount < 0) then begin
  panic('both inputs must be positive integers');
end;
  logical_right_shift_binary_number := to_binary_no_prefix(number);
  if shift_amount >= Length(logical_right_shift_binary_number) then begin
  exit('0b0');
end;
  logical_right_shift_shifted := copy(logical_right_shift_binary_number, 0+1, (Length(logical_right_shift_binary_number) - shift_amount - (0)));
  exit('0b' + logical_right_shift_shifted);
end;
function arithmetic_right_shift(number: integer; shift_amount: integer): string;
var
  arithmetic_right_shift_binary_number: string;
  arithmetic_right_shift_length: integer;
  arithmetic_right_shift_intermediate: integer;
  arithmetic_right_shift_bin_repr: string;
  arithmetic_right_shift_sign: string;
  arithmetic_right_shift_shifted: string;
begin
  if number >= 0 then begin
  arithmetic_right_shift_binary_number := '0' + to_binary_no_prefix(number);
end else begin
  arithmetic_right_shift_length := Length(to_binary_no_prefix(-number));
  arithmetic_right_shift_intermediate := abs_int(number) - pow2(arithmetic_right_shift_length);
  arithmetic_right_shift_bin_repr := to_binary_no_prefix(arithmetic_right_shift_intermediate);
  arithmetic_right_shift_binary_number := ('1' + repeat_char('0', arithmetic_right_shift_length - Length(arithmetic_right_shift_bin_repr))) + arithmetic_right_shift_bin_repr;
end;
  if shift_amount >= Length(arithmetic_right_shift_binary_number) then begin
  arithmetic_right_shift_sign := copy(arithmetic_right_shift_binary_number, 0+1, (1 - (0)));
  exit('0b' + repeat_char(arithmetic_right_shift_sign, Length(arithmetic_right_shift_binary_number)));
end;
  arithmetic_right_shift_sign := copy(arithmetic_right_shift_binary_number, 0+1, (1 - (0)));
  arithmetic_right_shift_shifted := copy(arithmetic_right_shift_binary_number, 0+1, (Length(arithmetic_right_shift_binary_number) - shift_amount - (0)));
  exit(('0b' + repeat_char(arithmetic_right_shift_sign, shift_amount)) + arithmetic_right_shift_shifted);
end;
procedure main();
begin
  writeln(logical_left_shift(17, 2));
  writeln(logical_right_shift(1983, 4));
  writeln(arithmetic_right_shift(-17, 2));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

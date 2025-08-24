{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  hex_digits: string;
function split_by_dot(split_by_dot_s: string): StrArray; forward;
function parse_decimal(parse_decimal_s: string): int64; forward;
function to_hex2(to_hex2_n: int64): string; forward;
function ipv4_to_decimal(ipv4_to_decimal_ipv4_address: string): int64; forward;
function alt_ipv4_to_decimal(alt_ipv4_to_decimal_ipv4_address: string): int64; forward;
function decimal_to_ipv4(decimal_to_ipv4_decimal_ipv4: int64): string; forward;
function split_by_dot(split_by_dot_s: string): StrArray;
var
  split_by_dot_res: array of string;
  split_by_dot_current: string;
  split_by_dot_i: int64;
  split_by_dot_c: string;
begin
  split_by_dot_res := [];
  split_by_dot_current := '';
  split_by_dot_i := 0;
  while split_by_dot_i < Length(split_by_dot_s) do begin
  split_by_dot_c := split_by_dot_s[split_by_dot_i+1];
  if split_by_dot_c = '.' then begin
  split_by_dot_res := concat(split_by_dot_res, StrArray([split_by_dot_current]));
  split_by_dot_current := '';
end else begin
  split_by_dot_current := split_by_dot_current + split_by_dot_c;
end;
  split_by_dot_i := split_by_dot_i + 1;
end;
  split_by_dot_res := concat(split_by_dot_res, StrArray([split_by_dot_current]));
  exit(split_by_dot_res);
end;
function parse_decimal(parse_decimal_s: string): int64;
var
  parse_decimal_value: int64;
  parse_decimal_i: int64;
  parse_decimal_c: string;
begin
  if Length(parse_decimal_s) = 0 then begin
  panic('Invalid IPv4 address format');
end;
  parse_decimal_value := 0;
  parse_decimal_i := 0;
  while parse_decimal_i < Length(parse_decimal_s) do begin
  parse_decimal_c := parse_decimal_s[parse_decimal_i+1];
  if (parse_decimal_c < '0') or (parse_decimal_c > '9') then begin
  panic('Invalid IPv4 address format');
end;
  parse_decimal_value := (parse_decimal_value * 10) + StrToInt(parse_decimal_c);
  parse_decimal_i := parse_decimal_i + 1;
end;
  exit(parse_decimal_value);
end;
function to_hex2(to_hex2_n: int64): string;
var
  to_hex2_x: int64;
  to_hex2_res: string;
  to_hex2_d: int64;
begin
  to_hex2_x := to_hex2_n;
  to_hex2_res := '';
  while to_hex2_x > 0 do begin
  to_hex2_d := to_hex2_x mod 16;
  to_hex2_res := hex_digits[to_hex2_d+1] + to_hex2_res;
  to_hex2_x := to_hex2_x div 16;
end;
  while Length(to_hex2_res) < 2 do begin
  to_hex2_res := '0' + to_hex2_res;
end;
  exit(to_hex2_res);
end;
function ipv4_to_decimal(ipv4_to_decimal_ipv4_address: string): int64;
var
  ipv4_to_decimal_parts: StrArray;
  ipv4_to_decimal_result_: int64;
  ipv4_to_decimal_i: int64;
  ipv4_to_decimal_oct: int64;
begin
  ipv4_to_decimal_parts := split_by_dot(ipv4_to_decimal_ipv4_address);
  if Length(ipv4_to_decimal_parts) <> 4 then begin
  panic('Invalid IPv4 address format');
end;
  ipv4_to_decimal_result_ := 0;
  ipv4_to_decimal_i := 0;
  while ipv4_to_decimal_i < 4 do begin
  ipv4_to_decimal_oct := parse_decimal(ipv4_to_decimal_parts[ipv4_to_decimal_i]);
  if (ipv4_to_decimal_oct < 0) or (ipv4_to_decimal_oct > 255) then begin
  panic('Invalid IPv4 octet ' + IntToStr(ipv4_to_decimal_oct));
end;
  ipv4_to_decimal_result_ := (ipv4_to_decimal_result_ * 256) + ipv4_to_decimal_oct;
  ipv4_to_decimal_i := ipv4_to_decimal_i + 1;
end;
  exit(ipv4_to_decimal_result_);
end;
function alt_ipv4_to_decimal(alt_ipv4_to_decimal_ipv4_address: string): int64;
var
  alt_ipv4_to_decimal_parts: StrArray;
  alt_ipv4_to_decimal_hex_str: string;
  alt_ipv4_to_decimal_i: int64;
  alt_ipv4_to_decimal_oct: int64;
  alt_ipv4_to_decimal_value: int64;
  alt_ipv4_to_decimal_k: int64;
  alt_ipv4_to_decimal_c: string;
  alt_ipv4_to_decimal_digit: int64;
  alt_ipv4_to_decimal_j: int64;
begin
  alt_ipv4_to_decimal_parts := split_by_dot(alt_ipv4_to_decimal_ipv4_address);
  if Length(alt_ipv4_to_decimal_parts) <> 4 then begin
  panic('Invalid IPv4 address format');
end;
  alt_ipv4_to_decimal_hex_str := '';
  alt_ipv4_to_decimal_i := 0;
  while alt_ipv4_to_decimal_i < 4 do begin
  alt_ipv4_to_decimal_oct := parse_decimal(alt_ipv4_to_decimal_parts[alt_ipv4_to_decimal_i]);
  if (alt_ipv4_to_decimal_oct < 0) or (alt_ipv4_to_decimal_oct > 255) then begin
  panic('Invalid IPv4 octet ' + IntToStr(alt_ipv4_to_decimal_oct));
end;
  alt_ipv4_to_decimal_hex_str := alt_ipv4_to_decimal_hex_str + to_hex2(alt_ipv4_to_decimal_oct);
  alt_ipv4_to_decimal_i := alt_ipv4_to_decimal_i + 1;
end;
  alt_ipv4_to_decimal_value := 0;
  alt_ipv4_to_decimal_k := 0;
  while alt_ipv4_to_decimal_k < Length(alt_ipv4_to_decimal_hex_str) do begin
  alt_ipv4_to_decimal_c := alt_ipv4_to_decimal_hex_str[alt_ipv4_to_decimal_k+1];
  alt_ipv4_to_decimal_digit := 0 - 1;
  alt_ipv4_to_decimal_j := 0;
  while alt_ipv4_to_decimal_j < Length(hex_digits) do begin
  if hex_digits[alt_ipv4_to_decimal_j+1] = alt_ipv4_to_decimal_c then begin
  alt_ipv4_to_decimal_digit := alt_ipv4_to_decimal_j;
end;
  alt_ipv4_to_decimal_j := alt_ipv4_to_decimal_j + 1;
end;
  if alt_ipv4_to_decimal_digit < 0 then begin
  panic('Invalid hex digit');
end;
  alt_ipv4_to_decimal_value := (alt_ipv4_to_decimal_value * 16) + alt_ipv4_to_decimal_digit;
  alt_ipv4_to_decimal_k := alt_ipv4_to_decimal_k + 1;
end;
  exit(alt_ipv4_to_decimal_value);
end;
function decimal_to_ipv4(decimal_to_ipv4_decimal_ipv4: int64): string;
var
  decimal_to_ipv4_n: int64;
  decimal_to_ipv4_parts: array of string;
  decimal_to_ipv4_i: int64;
  decimal_to_ipv4_octet: int64;
  decimal_to_ipv4_res: string;
  decimal_to_ipv4_j: integer;
begin
  if (decimal_to_ipv4_decimal_ipv4 < 0) or (decimal_to_ipv4_decimal_ipv4 > 4294967295) then begin
  panic('Invalid decimal IPv4 address');
end;
  decimal_to_ipv4_n := decimal_to_ipv4_decimal_ipv4;
  decimal_to_ipv4_parts := [];
  decimal_to_ipv4_i := 0;
  while decimal_to_ipv4_i < 4 do begin
  decimal_to_ipv4_octet := decimal_to_ipv4_n mod 256;
  decimal_to_ipv4_parts := concat(decimal_to_ipv4_parts, StrArray([IntToStr(decimal_to_ipv4_octet)]));
  decimal_to_ipv4_n := decimal_to_ipv4_n div 256;
  decimal_to_ipv4_i := decimal_to_ipv4_i + 1;
end;
  decimal_to_ipv4_res := '';
  decimal_to_ipv4_j := Length(decimal_to_ipv4_parts) - 1;
  while decimal_to_ipv4_j >= 0 do begin
  decimal_to_ipv4_res := decimal_to_ipv4_res + decimal_to_ipv4_parts[decimal_to_ipv4_j];
  if decimal_to_ipv4_j > 0 then begin
  decimal_to_ipv4_res := decimal_to_ipv4_res + '.';
end;
  decimal_to_ipv4_j := decimal_to_ipv4_j - 1;
end;
  exit(decimal_to_ipv4_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  hex_digits := '0123456789abcdef';
  writeln(ipv4_to_decimal('192.168.0.1'));
  writeln(ipv4_to_decimal('10.0.0.255'));
  writeln(alt_ipv4_to_decimal('192.168.0.1'));
  writeln(alt_ipv4_to_decimal('10.0.0.255'));
  writeln(decimal_to_ipv4(3232235521));
  writeln(decimal_to_ipv4(167772415));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

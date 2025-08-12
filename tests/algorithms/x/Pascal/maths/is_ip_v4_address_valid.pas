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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  s: string;
  ip: string;
function split_by_dot(s: string): StrArray; forward;
function is_digit_str(s: string): boolean; forward;
function parse_decimal(s: string): integer; forward;
function is_ip_v4_address_valid(ip: string): boolean; forward;
function split_by_dot(s: string): StrArray;
var
  split_by_dot_res: array of string;
  split_by_dot_current: string;
  split_by_dot_i: integer;
  split_by_dot_c: string;
begin
  split_by_dot_res := [];
  split_by_dot_current := '';
  split_by_dot_i := 0;
  while split_by_dot_i < Length(s) do begin
  split_by_dot_c := s[split_by_dot_i+1];
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
function is_digit_str(s: string): boolean;
var
  is_digit_str_i: integer;
  is_digit_str_c: string;
begin
  if Length(s) = 0 then begin
  exit(false);
end;
  is_digit_str_i := 0;
  while is_digit_str_i < Length(s) do begin
  is_digit_str_c := s[is_digit_str_i+1];
  if (is_digit_str_c < '0') or (is_digit_str_c > '9') then begin
  exit(false);
end;
  is_digit_str_i := is_digit_str_i + 1;
end;
  exit(true);
end;
function parse_decimal(s: string): integer;
var
  parse_decimal_value: integer;
  parse_decimal_i: integer;
  parse_decimal_c: string;
begin
  parse_decimal_value := 0;
  parse_decimal_i := 0;
  while parse_decimal_i < Length(s) do begin
  parse_decimal_c := s[parse_decimal_i+1];
  parse_decimal_value := (parse_decimal_value * 10) + StrToInt(parse_decimal_c);
  parse_decimal_i := parse_decimal_i + 1;
end;
  exit(parse_decimal_value);
end;
function is_ip_v4_address_valid(ip: string): boolean;
var
  is_ip_v4_address_valid_octets: StrArray;
  is_ip_v4_address_valid_i: integer;
  is_ip_v4_address_valid_oct: string;
  is_ip_v4_address_valid_number: integer;
begin
  is_ip_v4_address_valid_octets := split_by_dot(ip);
  if Length(is_ip_v4_address_valid_octets) <> 4 then begin
  exit(false);
end;
  is_ip_v4_address_valid_i := 0;
  while is_ip_v4_address_valid_i < 4 do begin
  is_ip_v4_address_valid_oct := is_ip_v4_address_valid_octets[is_ip_v4_address_valid_i];
  if not is_digit_str(is_ip_v4_address_valid_oct) then begin
  exit(false);
end;
  is_ip_v4_address_valid_number := parse_decimal(is_ip_v4_address_valid_oct);
  if Length(IntToStr(is_ip_v4_address_valid_number)) <> Length(is_ip_v4_address_valid_oct) then begin
  exit(false);
end;
  if (is_ip_v4_address_valid_number < 0) or (is_ip_v4_address_valid_number > 255) then begin
  exit(false);
end;
  is_ip_v4_address_valid_i := is_ip_v4_address_valid_i + 1;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('192.168.0.23'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('192.256.15.8'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('172.100.0.8'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('255.256.0.256'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('1.2.33333333.4'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('1.2.-3.4'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('1.2.3'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('1.2.3.4.5'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('1.2.A.4'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('0.0.0.0'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('1.2.3.'), true)));
  writeln(LowerCase(BoolToStr(is_ip_v4_address_valid('1.2.3.05'), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

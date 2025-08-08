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
  nums: array of string;
  t: integer;
  num: string;
  octal: string;
function octal_to_hex(octal: string): string; forward;
function octal_to_hex(octal: string): string;
var
  octal_to_hex_s: string;
  octal_to_hex_j: integer;
  octal_to_hex_c: string;
  octal_to_hex_decimal: integer;
  octal_to_hex_k: integer;
  octal_to_hex_d: integer;
  octal_to_hex_hex_chars: string;
  octal_to_hex_hex: string;
  octal_to_hex_idx: integer;
begin
  octal_to_hex_s := octal;
  if ((Length(octal_to_hex_s) >= 2) and (octal_to_hex_s[0+1] = '0')) and (octal_to_hex_s[1+1] = 'o') then begin
  octal_to_hex_s := copy(octal_to_hex_s, 2+1, (Length(octal_to_hex_s) - (2)));
end;
  if Length(octal_to_hex_s) = 0 then begin
  panic('Empty string was passed to the function');
end;
  octal_to_hex_j := 0;
  while octal_to_hex_j < Length(octal_to_hex_s) do begin
  octal_to_hex_c := octal_to_hex_s[octal_to_hex_j+1];
  if (((((((octal_to_hex_c <> '0') and (octal_to_hex_c <> '1')) and (octal_to_hex_c <> '2')) and (octal_to_hex_c <> '3')) and (octal_to_hex_c <> '4')) and (octal_to_hex_c <> '5')) and (octal_to_hex_c <> '6')) and (octal_to_hex_c <> '7') then begin
  panic('Not a Valid Octal Number');
end;
  octal_to_hex_j := octal_to_hex_j + 1;
end;
  octal_to_hex_decimal := 0;
  octal_to_hex_k := 0;
  while octal_to_hex_k < Length(octal_to_hex_s) do begin
  octal_to_hex_d := StrToInt(octal_to_hex_s[octal_to_hex_k+1]);
  octal_to_hex_decimal := (octal_to_hex_decimal * 8) + octal_to_hex_d;
  octal_to_hex_k := octal_to_hex_k + 1;
end;
  octal_to_hex_hex_chars := '0123456789ABCDEF';
  if octal_to_hex_decimal = 0 then begin
  exit('0x');
end;
  octal_to_hex_hex := '';
  while octal_to_hex_decimal > 0 do begin
  octal_to_hex_idx := octal_to_hex_decimal mod 16;
  octal_to_hex_hex := octal_to_hex_hex_chars[octal_to_hex_idx+1] + octal_to_hex_hex;
  octal_to_hex_decimal := octal_to_hex_decimal div 16;
end;
  exit('0x' + octal_to_hex_hex);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  nums := ['030', '100', '247', '235', '007'];
  t := 0;
  while t < Length(nums) do begin
  num := nums[t];
  writeln(octal_to_hex(num));
  t := t + 1;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

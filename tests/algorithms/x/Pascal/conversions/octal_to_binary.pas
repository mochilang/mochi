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
  octal_number: string;
function octal_to_binary(octal_number: string): string; forward;
function octal_to_binary(octal_number: string): string;
var
  octal_to_binary_octal_digits: string;
  octal_to_binary_binary_number: string;
  octal_to_binary_i: integer;
  octal_to_binary_digit: string;
  octal_to_binary_valid: boolean;
  octal_to_binary_j: integer;
  octal_to_binary_value: integer;
  octal_to_binary_k: integer;
  octal_to_binary_binary_digit: string;
begin
  if Length(octal_number) = 0 then begin
  panic('Empty string was passed to the function');
end;
  octal_to_binary_octal_digits := '01234567';
  octal_to_binary_binary_number := '';
  octal_to_binary_i := 0;
  while octal_to_binary_i < Length(octal_number) do begin
  octal_to_binary_digit := octal_number[octal_to_binary_i+1];
  octal_to_binary_valid := false;
  octal_to_binary_j := 0;
  while octal_to_binary_j < Length(octal_to_binary_octal_digits) do begin
  if octal_to_binary_digit = octal_to_binary_octal_digits[octal_to_binary_j+1] then begin
  octal_to_binary_valid := true;
  break;
end;
  octal_to_binary_j := octal_to_binary_j + 1;
end;
  if not octal_to_binary_valid then begin
  panic('Non-octal value was passed to the function');
end;
  octal_to_binary_value := StrToInt(octal_to_binary_digit);
  octal_to_binary_k := 0;
  octal_to_binary_binary_digit := '';
  while octal_to_binary_k < 3 do begin
  octal_to_binary_binary_digit := IntToStr(octal_to_binary_value mod 2) + octal_to_binary_binary_digit;
  octal_to_binary_value := octal_to_binary_value div 2;
  octal_to_binary_k := octal_to_binary_k + 1;
end;
  octal_to_binary_binary_number := octal_to_binary_binary_number + octal_to_binary_binary_digit;
  octal_to_binary_i := octal_to_binary_i + 1;
end;
  exit(octal_to_binary_binary_number);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(octal_to_binary('17'));
  writeln(octal_to_binary('7'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

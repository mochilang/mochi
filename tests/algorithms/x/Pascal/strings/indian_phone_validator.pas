{$mode objfpc}{$modeswitch nestedprocvars}
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function all_digits(all_digits_s: string): boolean; forward;
function indian_phone_validator(indian_phone_validator_phone: string): boolean; forward;
function all_digits(all_digits_s: string): boolean;
var
  all_digits_i: integer;
  all_digits_c: string;
begin
  if Length(all_digits_s) = 0 then begin
  exit(false);
end;
  all_digits_i := 0;
  while all_digits_i < Length(all_digits_s) do begin
  all_digits_c := all_digits_s[all_digits_i+1];
  if (all_digits_c < '0') or (all_digits_c > '9') then begin
  exit(false);
end;
  all_digits_i := all_digits_i + 1;
end;
  exit(true);
end;
function indian_phone_validator(indian_phone_validator_phone: string): boolean;
var
  indian_phone_validator_s: string;
  indian_phone_validator_c: string;
  indian_phone_validator_first: string;
begin
  indian_phone_validator_s := indian_phone_validator_phone;
  if (Length(indian_phone_validator_s) >= 3) and (copy(indian_phone_validator_s, 1, 3) = '+91') then begin
  indian_phone_validator_s := copy(indian_phone_validator_s, 4, (Length(indian_phone_validator_s) - (3)));
  if Length(indian_phone_validator_s) > 0 then begin
  indian_phone_validator_c := indian_phone_validator_s[0+1];
  if (indian_phone_validator_c = '-') or (indian_phone_validator_c = ' ') then begin
  indian_phone_validator_s := copy(indian_phone_validator_s, 2, (Length(indian_phone_validator_s) - (1)));
end;
end;
end;
  if (Length(indian_phone_validator_s) > 0) and (indian_phone_validator_s[0+1] = '0') then begin
  indian_phone_validator_s := copy(indian_phone_validator_s, 2, (Length(indian_phone_validator_s) - (1)));
end;
  if (Length(indian_phone_validator_s) >= 2) and (copy(indian_phone_validator_s, 1, 2) = '91') then begin
  indian_phone_validator_s := copy(indian_phone_validator_s, 3, (Length(indian_phone_validator_s) - (2)));
end;
  if Length(indian_phone_validator_s) <> 10 then begin
  exit(false);
end;
  indian_phone_validator_first := indian_phone_validator_s[0+1];
  if not (((indian_phone_validator_first = '7') or (indian_phone_validator_first = '8')) or (indian_phone_validator_first = '9')) then begin
  exit(false);
end;
  if not all_digits(indian_phone_validator_s) then begin
  exit(false);
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(indian_phone_validator('+91123456789'), true)));
  writeln(LowerCase(BoolToStr(indian_phone_validator('+919876543210'), true)));
  writeln(LowerCase(BoolToStr(indian_phone_validator('01234567896'), true)));
  writeln(LowerCase(BoolToStr(indian_phone_validator('919876543218'), true)));
  writeln(LowerCase(BoolToStr(indian_phone_validator('+91-1234567899'), true)));
  writeln(LowerCase(BoolToStr(indian_phone_validator('+91-9876543218'), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

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
function contains(xs: array of int64; v: int64): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
function contains(xs: array of string; v: string): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  phone: string;
function starts_with(starts_with_s: string; starts_with_prefix: string): boolean; forward;
function all_digits(all_digits_s: string): boolean; forward;
function is_sri_lankan_phone_number(is_sri_lankan_phone_number_phone: string): boolean; forward;
function starts_with(starts_with_s: string; starts_with_prefix: string): boolean;
begin
  if Length(starts_with_s) < Length(starts_with_prefix) then begin
  exit(false);
end;
  exit(copy(starts_with_s, 1, (Length(starts_with_prefix) - (0))) = starts_with_prefix);
end;
function all_digits(all_digits_s: string): boolean;
var
  all_digits_i: integer;
  all_digits_c: string;
begin
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
function is_sri_lankan_phone_number(is_sri_lankan_phone_number_phone: string): boolean;
var
  is_sri_lankan_phone_number_p: string;
  is_sri_lankan_phone_number_second: string;
  is_sri_lankan_phone_number_allowed: array of string;
  is_sri_lankan_phone_number_idx: integer;
  is_sri_lankan_phone_number_sep: string;
  is_sri_lankan_phone_number_rest: string;
begin
  is_sri_lankan_phone_number_p := is_sri_lankan_phone_number_phone;
  if starts_with(is_sri_lankan_phone_number_p, '+94') then begin
  is_sri_lankan_phone_number_p := copy(is_sri_lankan_phone_number_p, 4, (Length(is_sri_lankan_phone_number_p) - (3)));
end else begin
  if starts_with(is_sri_lankan_phone_number_p, '0094') then begin
  is_sri_lankan_phone_number_p := copy(is_sri_lankan_phone_number_p, 5, (Length(is_sri_lankan_phone_number_p) - (4)));
end else begin
  if starts_with(is_sri_lankan_phone_number_p, '94') then begin
  is_sri_lankan_phone_number_p := copy(is_sri_lankan_phone_number_p, 3, (Length(is_sri_lankan_phone_number_p) - (2)));
end else begin
  if starts_with(is_sri_lankan_phone_number_p, '0') then begin
  is_sri_lankan_phone_number_p := copy(is_sri_lankan_phone_number_p, 2, (Length(is_sri_lankan_phone_number_p) - (1)));
end else begin
  exit(false);
end;
end;
end;
end;
  if (Length(is_sri_lankan_phone_number_p) <> 9) and (Length(is_sri_lankan_phone_number_p) <> 10) then begin
  exit(false);
end;
  if is_sri_lankan_phone_number_p[0+1] <> '7' then begin
  exit(false);
end;
  is_sri_lankan_phone_number_second := is_sri_lankan_phone_number_p[1+1];
  is_sri_lankan_phone_number_allowed := ['0', '1', '2', '4', '5', '6', '7', '8'];
  if not (contains(is_sri_lankan_phone_number_allowed, is_sri_lankan_phone_number_second)) then begin
  exit(false);
end;
  is_sri_lankan_phone_number_idx := 2;
  if Length(is_sri_lankan_phone_number_p) = 10 then begin
  is_sri_lankan_phone_number_sep := is_sri_lankan_phone_number_p[2+1];
  if (is_sri_lankan_phone_number_sep <> '-') and (is_sri_lankan_phone_number_sep <> ' ') then begin
  exit(false);
end;
  is_sri_lankan_phone_number_idx := 3;
end;
  if (Length(is_sri_lankan_phone_number_p) - is_sri_lankan_phone_number_idx) <> 7 then begin
  exit(false);
end;
  is_sri_lankan_phone_number_rest := copy(is_sri_lankan_phone_number_p, is_sri_lankan_phone_number_idx+1, (Length(is_sri_lankan_phone_number_p) - (is_sri_lankan_phone_number_idx)));
  exit(all_digits(is_sri_lankan_phone_number_rest));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  phone := '0094702343221';
  writeln(LowerCase(BoolToStr(is_sri_lankan_phone_number(phone), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

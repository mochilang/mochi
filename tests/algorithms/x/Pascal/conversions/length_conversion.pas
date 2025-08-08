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
  s: string;
  u: string;
  value: real;
  from_type: string;
  to_type: string;
function rstrip_s(s: string): string; forward;
function normalize_alias(u: string): string; forward;
function has_unit(u: string): boolean; forward;
function from_factor(u: string): real; forward;
function to_factor(u: string): real; forward;
function length_conversion(value: real; from_type: string; to_type: string): real; forward;
function rstrip_s(s: string): string;
begin
  if (Length(s) > 0) and (s[Length(s) - 1+1] = 's') then begin
  exit(copy(s, 0+1, (Length(s) - 1 - (0))));
end;
  exit(s);
end;
function normalize_alias(u: string): string;
begin
  if u = 'millimeter' then begin
  exit('mm');
end;
  if u = 'centimeter' then begin
  exit('cm');
end;
  if u = 'meter' then begin
  exit('m');
end;
  if u = 'kilometer' then begin
  exit('km');
end;
  if u = 'inch' then begin
  exit('in');
end;
  if u = 'inche' then begin
  exit('in');
end;
  if u = 'feet' then begin
  exit('ft');
end;
  if u = 'foot' then begin
  exit('ft');
end;
  if u = 'yard' then begin
  exit('yd');
end;
  if u = 'mile' then begin
  exit('mi');
end;
  exit(u);
end;
function has_unit(u: string): boolean;
begin
  exit((((((((u = 'mm') or (u = 'cm')) or (u = 'm')) or (u = 'km')) or (u = 'in')) or (u = 'ft')) or (u = 'yd')) or (u = 'mi'));
end;
function from_factor(u: string): real;
begin
  if u = 'mm' then begin
  exit(0.001);
end;
  if u = 'cm' then begin
  exit(0.01);
end;
  if u = 'm' then begin
  exit(1);
end;
  if u = 'km' then begin
  exit(1000);
end;
  if u = 'in' then begin
  exit(0.0254);
end;
  if u = 'ft' then begin
  exit(0.3048);
end;
  if u = 'yd' then begin
  exit(0.9144);
end;
  if u = 'mi' then begin
  exit(1609.34);
end;
  exit(0);
end;
function to_factor(u: string): real;
begin
  if u = 'mm' then begin
  exit(1000);
end;
  if u = 'cm' then begin
  exit(100);
end;
  if u = 'm' then begin
  exit(1);
end;
  if u = 'km' then begin
  exit(0.001);
end;
  if u = 'in' then begin
  exit(39.3701);
end;
  if u = 'ft' then begin
  exit(3.28084);
end;
  if u = 'yd' then begin
  exit(1.09361);
end;
  if u = 'mi' then begin
  exit(0.000621371);
end;
  exit(0);
end;
function length_conversion(value: real; from_type: string; to_type: string): real;
var
  length_conversion_new_from: string;
  length_conversion_new_to: string;
begin
  length_conversion_new_from := normalize_alias(rstrip_s(LowerCase(from_type)));
  length_conversion_new_to := normalize_alias(rstrip_s(LowerCase(to_type)));
  if not has_unit(length_conversion_new_from) then begin
  panic(('Invalid ''from_type'' value: ''' + from_type) + '''.' + #10 + 'Conversion abbreviations are: mm, cm, m, km, in, ft, yd, mi');
end;
  if not has_unit(length_conversion_new_to) then begin
  panic(('Invalid ''to_type'' value: ''' + to_type) + '''.' + #10 + 'Conversion abbreviations are: mm, cm, m, km, in, ft, yd, mi');
end;
  exit((value * from_factor(length_conversion_new_from)) * to_factor(length_conversion_new_to));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(length_conversion(4, 'METER', 'FEET'));
  writeln(length_conversion(1, 'kilometer', 'inch'));
  writeln(length_conversion(2, 'feet', 'meter'));
  writeln(length_conversion(2, 'centimeter', 'millimeter'));
  writeln(length_conversion(4, 'yard', 'kilometer'));
  writeln(length_conversion(3, 'foot', 'inch'));
  writeln(length_conversion(3, 'mm', 'in'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

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
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function split_with_sep(split_with_sep_s: string; split_with_sep_sep: string): StrArray; forward;
function split(split_s: string): StrArray; forward;
function split_with_sep(split_with_sep_s: string; split_with_sep_sep: string): StrArray;
var
  split_with_sep_parts: array of string;
  split_with_sep_last: integer;
  split_with_sep_i: integer;
  split_with_sep_ch: string;
begin
  split_with_sep_parts := [];
  split_with_sep_last := 0;
  split_with_sep_i := 0;
  while split_with_sep_i < Length(split_with_sep_s) do begin
  split_with_sep_ch := copy(split_with_sep_s, split_with_sep_i+1, (split_with_sep_i + 1 - (split_with_sep_i)));
  if split_with_sep_ch = split_with_sep_sep then begin
  split_with_sep_parts := concat(split_with_sep_parts, StrArray([copy(split_with_sep_s, split_with_sep_last+1, (split_with_sep_i - (split_with_sep_last)))]));
  split_with_sep_last := split_with_sep_i + 1;
end;
  if (split_with_sep_i + 1) = Length(split_with_sep_s) then begin
  split_with_sep_parts := concat(split_with_sep_parts, StrArray([copy(split_with_sep_s, split_with_sep_last+1, (split_with_sep_i + 1 - (split_with_sep_last)))]));
end;
  split_with_sep_i := split_with_sep_i + 1;
end;
  exit(split_with_sep_parts);
end;
function split(split_s: string): StrArray;
begin
  exit(split_with_sep(split_s, ' '));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_to_str(split_with_sep('apple#banana#cherry#orange', '#')));
  writeln(list_to_str(split('Hello there')));
  writeln(list_to_str(split_with_sep('11/22/63', '/')));
  writeln(list_to_str(split_with_sep('12:43:39', ':')));
  writeln(list_to_str(split_with_sep(';abbb;;c;', ';')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

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
  lowercase: string;
  uppercase: string;
function index_of(index_of_s: string; index_of_c: string): integer; forward;
function is_alpha(is_alpha_c: string): boolean; forward;
function to_upper(to_upper_c: string): string; forward;
function wave(wave_txt: string): StrArray; forward;
function index_of(index_of_s: string; index_of_c: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(index_of_s) do begin
  if copy(index_of_s, index_of_i+1, (index_of_i + 1 - (index_of_i))) = index_of_c then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function is_alpha(is_alpha_c: string): boolean;
begin
  exit((index_of(lowercase, is_alpha_c) >= 0) or (index_of(uppercase, is_alpha_c) >= 0));
end;
function to_upper(to_upper_c: string): string;
var
  to_upper_idx: integer;
begin
  to_upper_idx := index_of(lowercase, to_upper_c);
  if to_upper_idx >= 0 then begin
  exit(copy(uppercase, to_upper_idx+1, (to_upper_idx + 1 - (to_upper_idx))));
end;
  exit(to_upper_c);
end;
function wave(wave_txt: string): StrArray;
var
  wave_result_: array of string;
  wave_i: integer;
  wave_ch: string;
  wave_prefix: string;
  wave_suffix: string;
begin
  wave_result_ := [];
  wave_i := 0;
  while wave_i < Length(wave_txt) do begin
  wave_ch := copy(wave_txt, wave_i+1, (wave_i + 1 - (wave_i)));
  if is_alpha(wave_ch) then begin
  wave_prefix := copy(wave_txt, 1, (wave_i - (0)));
  wave_suffix := copy(wave_txt, wave_i + 1+1, (Length(wave_txt) - (wave_i + 1)));
  wave_result_ := concat(wave_result_, StrArray([(wave_prefix + to_upper(wave_ch)) + wave_suffix]));
end;
  wave_i := wave_i + 1;
end;
  exit(wave_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  lowercase := 'abcdefghijklmnopqrstuvwxyz';
  uppercase := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  writeln(list_to_str(wave('cat')));
  writeln(list_to_str(wave('one')));
  writeln(list_to_str(wave('book')));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

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
function index_of(index_of_s: string; index_of_ch: string): integer; forward;
function lower(lower_word: string): string; forward;
function index_of(index_of_s: string; index_of_ch: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(index_of_s) do begin
  if index_of_s[index_of_i+1] = index_of_ch then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function lower(lower_word: string): string;
var
  lower_upper: string;
  lower_lower_chars: string;
  lower_result_: string;
  lower_i: integer;
  lower_c: string;
  lower_idx: integer;
begin
  lower_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  lower_lower_chars := 'abcdefghijklmnopqrstuvwxyz';
  lower_result_ := '';
  lower_i := 0;
  while lower_i < Length(lower_word) do begin
  lower_c := lower_word[lower_i+1];
  lower_idx := index_of(lower_upper, lower_c);
  if lower_idx >= 0 then begin
  lower_result_ := lower_result_ + copy(lower_lower_chars, lower_idx+1, (lower_idx + 1 - (lower_idx)));
end else begin
  lower_result_ := lower_result_ + lower_c;
end;
  lower_i := lower_i + 1;
end;
  exit(lower_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase('wow'));
  writeln(LowerCase('HellZo'));
  writeln(LowerCase('WHAT'));
  writeln(LowerCase('wh[]32'));
  writeln(LowerCase('whAT'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

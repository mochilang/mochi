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
function upper(upper_word: string): string; forward;
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
function upper(upper_word: string): string;
var
  upper_lower_chars: string;
  upper_upper_chars: string;
  upper_result_: string;
  upper_i: integer;
  upper_c: string;
  upper_idx: integer;
begin
  upper_lower_chars := 'abcdefghijklmnopqrstuvwxyz';
  upper_upper_chars := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  upper_result_ := '';
  upper_i := 0;
  while upper_i < Length(upper_word) do begin
  upper_c := upper_word[upper_i+1];
  upper_idx := index_of(upper_lower_chars, upper_c);
  if upper_idx >= 0 then begin
  upper_result_ := upper_result_ + copy(upper_upper_chars, upper_idx+1, (upper_idx + 1 - (upper_idx)));
end else begin
  upper_result_ := upper_result_ + upper_c;
end;
  upper_i := upper_i + 1;
end;
  exit(upper_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(UpperCase('wow'));
  writeln(UpperCase('Hello'));
  writeln(UpperCase('WHAT'));
  writeln(UpperCase('wh[]32'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

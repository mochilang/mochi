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
  vowels: string;
function is_vowel(is_vowel_c: string): boolean; forward;
function count_vowels(count_vowels_s: string): integer; forward;
procedure show(show_s: string); forward;
function is_vowel(is_vowel_c: string): boolean;
var
  is_vowel_i: integer;
begin
  is_vowel_i := 0;
  while is_vowel_i < Length(vowels) do begin
  if vowels[is_vowel_i+1] = is_vowel_c then begin
  exit(true);
end;
  is_vowel_i := is_vowel_i + 1;
end;
  exit(false);
end;
function count_vowels(count_vowels_s: string): integer;
var
  count_vowels_count: integer;
  count_vowels_i: integer;
  count_vowels_ch: string;
begin
  count_vowels_count := 0;
  count_vowels_i := 0;
  while count_vowels_i < Length(count_vowels_s) do begin
  count_vowels_ch := count_vowels_s[count_vowels_i+1];
  if is_vowel(count_vowels_ch) then begin
  count_vowels_count := count_vowels_count + 1;
end;
  count_vowels_i := count_vowels_i + 1;
end;
  exit(count_vowels_count);
end;
procedure show(show_s: string);
begin
  writeln(IntToStr(count_vowels(show_s)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  vowels := 'aeiouAEIOU';
  show('hello world');
  show('HELLO WORLD');
  show('123 hello world');
  show('');
  show('a quick brown fox');
  show('the quick BROWN fox');
  show('PYTHON');
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

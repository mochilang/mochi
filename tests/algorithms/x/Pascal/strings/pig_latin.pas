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
  VOWELS: string;
function strip(strip_s: string): string; forward;
function is_vowel(is_vowel_c: string): boolean; forward;
function pig_latin(pig_latin_word: string): string; forward;
function strip(strip_s: string): string;
var
  strip_start: integer;
  strip_end_: integer;
begin
  strip_start := 0;
  strip_end_ := Length(strip_s);
  while (strip_start < strip_end_) and (copy(strip_s, strip_start+1, (strip_start + 1 - (strip_start))) = ' ') do begin
  strip_start := strip_start + 1;
end;
  while (strip_end_ > strip_start) and (copy(strip_s, strip_end_ - 1+1, (strip_end_ - (strip_end_ - 1))) = ' ') do begin
  strip_end_ := strip_end_ - 1;
end;
  exit(copy(strip_s, strip_start+1, (strip_end_ - (strip_start))));
end;
function is_vowel(is_vowel_c: string): boolean;
var
  is_vowel_i: integer;
begin
  is_vowel_i := 0;
  while is_vowel_i < Length(VOWELS) do begin
  if is_vowel_c = copy(VOWELS, is_vowel_i+1, (is_vowel_i + 1 - (is_vowel_i))) then begin
  exit(true);
end;
  is_vowel_i := is_vowel_i + 1;
end;
  exit(false);
end;
function pig_latin(pig_latin_word: string): string;
var
  pig_latin_trimmed: string;
  pig_latin_w: string;
  pig_latin_first: string;
  pig_latin_i: integer;
  pig_latin_ch: string;
begin
  pig_latin_trimmed := strip(pig_latin_word);
  if Length(pig_latin_trimmed) = 0 then begin
  exit('');
end;
  pig_latin_w := LowerCase(pig_latin_trimmed);
  pig_latin_first := copy(pig_latin_w, 1, 1);
  if is_vowel(pig_latin_first) then begin
  exit(pig_latin_w + 'way');
end;
  pig_latin_i := 0;
  while pig_latin_i < Length(pig_latin_w) do begin
  pig_latin_ch := copy(pig_latin_w, pig_latin_i+1, (pig_latin_i + 1 - (pig_latin_i)));
  if is_vowel(pig_latin_ch) then begin
  break;
end;
  pig_latin_i := pig_latin_i + 1;
end;
  exit((copy(pig_latin_w, pig_latin_i+1, (Length(pig_latin_w) - (pig_latin_i))) + copy(pig_latin_w, 1, (pig_latin_i - (0)))) + 'ay');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  VOWELS := 'aeiou';
  writeln('pig_latin(''friends'') = ' + pig_latin('friends'));
  writeln('pig_latin(''smile'') = ' + pig_latin('smile'));
  writeln('pig_latin(''eat'') = ' + pig_latin('eat'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

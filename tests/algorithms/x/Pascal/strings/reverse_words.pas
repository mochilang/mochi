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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function split_words(split_words_s: string): StrArray; forward;
function reverse_words(reverse_words_input_str: string): string; forward;
procedure main(); forward;
function split_words(split_words_s: string): StrArray;
var
  split_words_words: array of string;
  split_words_current: string;
  split_words_i: integer;
  split_words_ch: string;
begin
  split_words_words := [];
  split_words_current := '';
  split_words_i := 0;
  while split_words_i < Length(split_words_s) do begin
  split_words_ch := split_words_s[split_words_i+1];
  if split_words_ch = ' ' then begin
  if Length(split_words_current) > 0 then begin
  split_words_words := concat(split_words_words, StrArray([split_words_current]));
  split_words_current := '';
end;
end else begin
  split_words_current := split_words_current + split_words_ch;
end;
  split_words_i := split_words_i + 1;
end;
  if Length(split_words_current) > 0 then begin
  split_words_words := concat(split_words_words, StrArray([split_words_current]));
end;
  exit(split_words_words);
end;
function reverse_words(reverse_words_input_str: string): string;
var
  reverse_words_words: StrArray;
  reverse_words_res: string;
  reverse_words_i: integer;
begin
  reverse_words_words := split_words(reverse_words_input_str);
  reverse_words_res := '';
  reverse_words_i := Length(reverse_words_words) - 1;
  while reverse_words_i >= 0 do begin
  reverse_words_res := reverse_words_res + reverse_words_words[reverse_words_i];
  if reverse_words_i > 0 then begin
  reverse_words_res := reverse_words_res + ' ';
end;
  reverse_words_i := reverse_words_i - 1;
end;
  exit(reverse_words_res);
end;
procedure main();
begin
  writeln(reverse_words('I love Python'));
  writeln(reverse_words('I     Love          Python'));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

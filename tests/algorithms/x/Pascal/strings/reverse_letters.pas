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
function split(split_s: string; split_sep: string): StrArray; forward;
function join_with_space(join_with_space_xs: StrArray): string; forward;
function reverse_str(reverse_str_s: string): string; forward;
function reverse_letters(reverse_letters_sentence: string; reverse_letters_length_: integer): string; forward;
procedure test_reverse_letters(); forward;
procedure main(); forward;
function split(split_s: string; split_sep: string): StrArray;
var
  split_res: array of string;
  split_current: string;
  split_i: integer;
  split_ch: string;
begin
  split_res := [];
  split_current := '';
  split_i := 0;
  while split_i < Length(split_s) do begin
  split_ch := split_s[split_i+1];
  if split_ch = split_sep then begin
  split_res := concat(split_res, StrArray([split_current]));
  split_current := '';
end else begin
  split_current := split_current + split_ch;
end;
  split_i := split_i + 1;
end;
  split_res := concat(split_res, StrArray([split_current]));
  exit(split_res);
end;
function join_with_space(join_with_space_xs: StrArray): string;
var
  join_with_space_s: string;
  join_with_space_i: integer;
begin
  join_with_space_s := '';
  join_with_space_i := 0;
  while join_with_space_i < Length(join_with_space_xs) do begin
  join_with_space_s := join_with_space_s + join_with_space_xs[join_with_space_i];
  if (join_with_space_i + 1) < Length(join_with_space_xs) then begin
  join_with_space_s := join_with_space_s + ' ';
end;
  join_with_space_i := join_with_space_i + 1;
end;
  exit(join_with_space_s);
end;
function reverse_str(reverse_str_s: string): string;
var
  reverse_str_res: string;
  reverse_str_i: integer;
begin
  reverse_str_res := '';
  reverse_str_i := Length(reverse_str_s) - 1;
  while reverse_str_i >= 0 do begin
  reverse_str_res := reverse_str_res + reverse_str_s[reverse_str_i+1];
  reverse_str_i := reverse_str_i - 1;
end;
  exit(reverse_str_res);
end;
function reverse_letters(reverse_letters_sentence: string; reverse_letters_length_: integer): string;
var
  reverse_letters_words: StrArray;
  reverse_letters_result_: array of string;
  reverse_letters_i: integer;
  reverse_letters_word: string;
begin
  reverse_letters_words := split(reverse_letters_sentence, ' ');
  reverse_letters_result_ := [];
  reverse_letters_i := 0;
  while reverse_letters_i < Length(reverse_letters_words) do begin
  reverse_letters_word := reverse_letters_words[reverse_letters_i];
  if Length(reverse_letters_word) > reverse_letters_length_ then begin
  reverse_letters_result_ := concat(reverse_letters_result_, StrArray([reverse_str(reverse_letters_word)]));
end else begin
  reverse_letters_result_ := concat(reverse_letters_result_, StrArray([reverse_letters_word]));
end;
  reverse_letters_i := reverse_letters_i + 1;
end;
  exit(join_with_space(reverse_letters_result_));
end;
procedure test_reverse_letters();
begin
  if reverse_letters('Hey wollef sroirraw', 3) <> 'Hey fellow warriors' then begin
  panic('test1 failed');
end;
  if reverse_letters('nohtyP is nohtyP', 2) <> 'Python is Python' then begin
  panic('test2 failed');
end;
  if reverse_letters('1 12 123 1234 54321 654321', 0) <> '1 21 321 4321 12345 123456' then begin
  panic('test3 failed');
end;
  if reverse_letters('racecar', 0) <> 'racecar' then begin
  panic('test4 failed');
end;
end;
procedure main();
begin
  test_reverse_letters();
  writeln(reverse_letters('Hey wollef sroirraw', 3));
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

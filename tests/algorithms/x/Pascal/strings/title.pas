{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, StrUtils;
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
  lower: string;
  upper: string;
function index_of(index_of_s: string; index_of_ch: string): integer; forward;
function to_title_case(to_title_case_word: string): string; forward;
function split_words(split_words_s: string): StrArray; forward;
function sentence_to_title_case(sentence_to_title_case_sentence: string): string; forward;
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
function to_title_case(to_title_case_word: string): string;
var
  to_title_case_first: string;
  to_title_case_idx: integer;
  to_title_case_result_: string;
  to_title_case_i: integer;
  to_title_case_ch: string;
  to_title_case_uidx: integer;
begin
  if Length(to_title_case_word) = 0 then begin
  exit('');
end;
  to_title_case_first := copy(to_title_case_word, 1, 1);
  to_title_case_idx := index_of(lower, to_title_case_first);
  to_title_case_result_ := IfThen(to_title_case_idx >= 0, copy(upper, to_title_case_idx+1, (to_title_case_idx + 1 - (to_title_case_idx))), to_title_case_first);
  to_title_case_i := 1;
  while to_title_case_i < Length(to_title_case_word) do begin
  to_title_case_ch := copy(to_title_case_word, to_title_case_i+1, (to_title_case_i + 1 - (to_title_case_i)));
  to_title_case_uidx := index_of(upper, to_title_case_ch);
  if to_title_case_uidx >= 0 then begin
  to_title_case_result_ := to_title_case_result_ + copy(lower, to_title_case_uidx+1, (to_title_case_uidx + 1 - (to_title_case_uidx)));
end else begin
  to_title_case_result_ := to_title_case_result_ + to_title_case_ch;
end;
  to_title_case_i := to_title_case_i + 1;
end;
  exit(to_title_case_result_);
end;
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
function sentence_to_title_case(sentence_to_title_case_sentence: string): string;
var
  sentence_to_title_case_words: StrArray;
  sentence_to_title_case_res: string;
  sentence_to_title_case_i: integer;
begin
  sentence_to_title_case_words := split_words(sentence_to_title_case_sentence);
  sentence_to_title_case_res := '';
  sentence_to_title_case_i := 0;
  while sentence_to_title_case_i < Length(sentence_to_title_case_words) do begin
  sentence_to_title_case_res := sentence_to_title_case_res + to_title_case(sentence_to_title_case_words[sentence_to_title_case_i]);
  if (sentence_to_title_case_i + 1) < Length(sentence_to_title_case_words) then begin
  sentence_to_title_case_res := sentence_to_title_case_res + ' ';
end;
  sentence_to_title_case_i := sentence_to_title_case_i + 1;
end;
  exit(sentence_to_title_case_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  lower := 'abcdefghijklmnopqrstuvwxyz';
  upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  writeln(to_title_case('Aakash'));
  writeln(to_title_case('aakash'));
  writeln(to_title_case('AAKASH'));
  writeln(to_title_case('aAkAsH'));
  writeln(sentence_to_title_case('Aakash Giri'));
  writeln(sentence_to_title_case('aakash giri'));
  writeln(sentence_to_title_case('AAKASH GIRI'));
  writeln(sentence_to_title_case('aAkAsH gIrI'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
var _dataDir: string = '/workspace/mochi/tests/github/TheAlgorithms/Mochi/project_euler/problem_042';
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
function _read_file(path: string): string;
var f: Text; line, out, pth: string;
begin
  pth := path;
  if (_dataDir <> '') and (not FileExists(pth)) then pth := _dataDir + '/' + path;
  out := '';
  if FileExists(pth) then begin
    Assign(f, pth);
    Reset(f);
    while not EOF(f) do begin
      ReadLn(f, line);
      if out <> '' then out := out + #10;
      out := out + line;
    end;
    Close(f);
  end;
  _read_file := out;
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
function triangular_numbers(triangular_numbers_limit: integer): IntArray; forward;
function parse_words(parse_words_text: string): StrArray; forward;
function word_value(word_value_word: string): integer; forward;
function contains(contains_xs: IntArray; contains_target: integer): boolean; forward;
function solution(): integer; forward;
function triangular_numbers(triangular_numbers_limit: integer): IntArray;
var
  triangular_numbers_res: array of integer;
  triangular_numbers_n: integer;
begin
  triangular_numbers_res := [];
  triangular_numbers_n := 1;
  while triangular_numbers_n <= triangular_numbers_limit do begin
  triangular_numbers_res := concat(triangular_numbers_res, [(triangular_numbers_n * (triangular_numbers_n + 1)) div 2]);
  triangular_numbers_n := triangular_numbers_n + 1;
end;
  exit(triangular_numbers_res);
end;
function parse_words(parse_words_text: string): StrArray;
var
  parse_words_words: array of string;
  parse_words_current: string;
  parse_words_i: integer;
  parse_words_c: string;
begin
  parse_words_words := [];
  parse_words_current := '';
  parse_words_i := 0;
  while parse_words_i < Length(parse_words_text) do begin
  parse_words_c := copy(parse_words_text, parse_words_i+1, (parse_words_i + 1 - (parse_words_i)));
  if parse_words_c = ',' then begin
  parse_words_words := concat(parse_words_words, StrArray([parse_words_current]));
  parse_words_current := '';
end else begin
  if parse_words_c = '"' then begin
end else begin
  if (parse_words_c = #13) or (parse_words_c = #10) then begin
end else begin
  parse_words_current := parse_words_current + parse_words_c;
end;
end;
end;
  parse_words_i := parse_words_i + 1;
end;
  if Length(parse_words_current) > 0 then begin
  parse_words_words := concat(parse_words_words, StrArray([parse_words_current]));
end;
  exit(parse_words_words);
end;
function word_value(word_value_word: string): integer;
var
  word_value_total: integer;
  word_value_i: integer;
begin
  word_value_total := 0;
  word_value_i := 0;
  while word_value_i < Length(word_value_word) do begin
  word_value_total := (word_value_total + Ord(word_value_word[word_value_i+1])) - 64;
  word_value_i := word_value_i + 1;
end;
  exit(word_value_total);
end;
function contains(contains_xs: IntArray; contains_target: integer): boolean;
var
  contains_x: integer;
begin
  for contains_x in contains_xs do begin
  if contains_x = contains_target then begin
  exit(true);
end;
end;
  exit(false);
end;
function solution(): integer;
var
  solution_text: string;
  solution_words: StrArray;
  solution_tri: IntArray;
  solution_count: integer;
  solution_w: string;
  solution_v: integer;
begin
  solution_text := _read_file('words.txt');
  solution_words := parse_words(solution_text);
  solution_tri := triangular_numbers(100);
  solution_count := 0;
  for solution_w in solution_words do begin
  solution_v := word_value(solution_w);
  if contains(solution_tri, solution_v) then begin
  solution_count := solution_count + 1;
end;
end;
  exit(solution_count);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(solution()));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

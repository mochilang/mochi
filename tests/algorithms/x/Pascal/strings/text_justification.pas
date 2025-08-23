{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
type IntArray = array of integer;
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
function repeat_str(repeat_str_s: string; repeat_str_count: integer): string; forward;
function split_words(split_words_s: string): StrArray; forward;
function justify_line(justify_line_line: StrArray; justify_line_width: integer; justify_line_max_width: integer): string; forward;
function text_justification(text_justification_word: string; text_justification_max_width: integer): StrArray; forward;
function repeat_str(repeat_str_s: string; repeat_str_count: integer): string;
var
  repeat_str_res: string;
  repeat_str_i: integer;
begin
  repeat_str_res := '';
  repeat_str_i := 0;
  while repeat_str_i < repeat_str_count do begin
  repeat_str_res := repeat_str_res + repeat_str_s;
  repeat_str_i := repeat_str_i + 1;
end;
  exit(repeat_str_res);
end;
function split_words(split_words_s: string): StrArray;
var
  split_words_res: array of string;
  split_words_current: string;
  split_words_i: integer;
  split_words_ch: string;
begin
  split_words_res := [];
  split_words_current := '';
  split_words_i := 0;
  while split_words_i < Length(split_words_s) do begin
  split_words_ch := copy(split_words_s, split_words_i+1, (split_words_i + 1 - (split_words_i)));
  if split_words_ch = ' ' then begin
  if split_words_current <> '' then begin
  split_words_res := concat(split_words_res, StrArray([split_words_current]));
  split_words_current := '';
end;
end else begin
  split_words_current := split_words_current + split_words_ch;
end;
  split_words_i := split_words_i + 1;
end;
  if split_words_current <> '' then begin
  split_words_res := concat(split_words_res, StrArray([split_words_current]));
end;
  exit(split_words_res);
end;
function justify_line(justify_line_line: StrArray; justify_line_width: integer; justify_line_max_width: integer): string;
var
  justify_line_overall_spaces_count: integer;
  justify_line_words_count: integer;
  justify_line_spaces_to_insert_between_words: integer;
  justify_line_num_spaces_between_words_list: array of integer;
  justify_line_base: integer;
  justify_line_extra: integer;
  justify_line_i: integer;
  justify_line_spaces: integer;
  justify_line_aligned: string;
begin
  justify_line_overall_spaces_count := justify_line_max_width - justify_line_width;
  justify_line_words_count := Length(justify_line_line);
  if justify_line_words_count = 1 then begin
  exit(justify_line_line[0] + repeat_str(' ', justify_line_overall_spaces_count));
end;
  justify_line_spaces_to_insert_between_words := justify_line_words_count - 1;
  justify_line_num_spaces_between_words_list := [];
  justify_line_base := justify_line_overall_spaces_count div justify_line_spaces_to_insert_between_words;
  justify_line_extra := justify_line_overall_spaces_count mod justify_line_spaces_to_insert_between_words;
  justify_line_i := 0;
  while justify_line_i < justify_line_spaces_to_insert_between_words do begin
  justify_line_spaces := justify_line_base;
  if justify_line_i < justify_line_extra then begin
  justify_line_spaces := justify_line_spaces + 1;
end;
  justify_line_num_spaces_between_words_list := concat(justify_line_num_spaces_between_words_list, [justify_line_spaces]);
  justify_line_i := justify_line_i + 1;
end;
  justify_line_aligned := '';
  justify_line_i := 0;
  while justify_line_i < justify_line_spaces_to_insert_between_words do begin
  justify_line_aligned := (justify_line_aligned + justify_line_line[justify_line_i]) + repeat_str(' ', justify_line_num_spaces_between_words_list[justify_line_i]);
  justify_line_i := justify_line_i + 1;
end;
  justify_line_aligned := justify_line_aligned + justify_line_line[justify_line_spaces_to_insert_between_words];
  exit(justify_line_aligned);
end;
function text_justification(text_justification_word: string; text_justification_max_width: integer): StrArray;
var
  text_justification_words: StrArray;
  text_justification_answer: array of string;
  text_justification_line: array of string;
  text_justification_width: integer;
  text_justification_idx: integer;
  text_justification_w: string;
  text_justification_remaining_spaces: integer;
  text_justification_last_line: string;
  text_justification_j: integer;
begin
  text_justification_words := split_words(text_justification_word);
  text_justification_answer := [];
  text_justification_line := [];
  text_justification_width := 0;
  text_justification_idx := 0;
  while text_justification_idx < Length(text_justification_words) do begin
  text_justification_w := text_justification_words[text_justification_idx];
  if ((text_justification_width + Length(text_justification_w)) + Length(text_justification_line)) <= text_justification_max_width then begin
  text_justification_line := concat(text_justification_line, StrArray([text_justification_w]));
  text_justification_width := text_justification_width + Length(text_justification_w);
end else begin
  text_justification_answer := concat(text_justification_answer, StrArray([justify_line(text_justification_line, text_justification_width, text_justification_max_width)]));
  text_justification_line := [text_justification_w];
  text_justification_width := Length(text_justification_w);
end;
  text_justification_idx := text_justification_idx + 1;
end;
  text_justification_remaining_spaces := (text_justification_max_width - text_justification_width) - Length(text_justification_line);
  text_justification_last_line := '';
  text_justification_j := 0;
  while text_justification_j < Length(text_justification_line) do begin
  if text_justification_j > 0 then begin
  text_justification_last_line := text_justification_last_line + ' ';
end;
  text_justification_last_line := text_justification_last_line + text_justification_line[text_justification_j];
  text_justification_j := text_justification_j + 1;
end;
  text_justification_last_line := text_justification_last_line + repeat_str(' ', text_justification_remaining_spaces + 1);
  text_justification_answer := concat(text_justification_answer, StrArray([text_justification_last_line]));
  exit(text_justification_answer);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_to_str(text_justification('This is an example of text justification.', 16)));
  writeln(list_to_str(text_justification('Two roads diverged in a yellow wood', 16)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

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
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function parse_names(parse_names_line: string): StrArray; forward;
function insertion_sort(insertion_sort_arr: StrArray): StrArray; forward;
function letter_value(letter_value_ch: string): int64; forward;
function name_score(name_score_name: string): int64; forward;
procedure main(); forward;
function parse_names(parse_names_line: string): StrArray;
var
  parse_names_names: array of string;
  parse_names_current: string;
  parse_names_i: int64;
  parse_names_ch: string;
begin
  parse_names_names := [];
  parse_names_current := '';
  parse_names_i := 0;
  while parse_names_i < Length(parse_names_line) do begin
  parse_names_ch := copy(parse_names_line, parse_names_i+1, (parse_names_i + 1 - (parse_names_i)));
  if parse_names_ch = ',' then begin
  parse_names_names := concat(parse_names_names, StrArray([parse_names_current]));
  parse_names_current := '';
end else begin
  if parse_names_ch <> '"' then begin
  parse_names_current := parse_names_current + parse_names_ch;
end;
end;
  parse_names_i := parse_names_i + 1;
end;
  parse_names_names := concat(parse_names_names, StrArray([parse_names_current]));
  exit(parse_names_names);
end;
function insertion_sort(insertion_sort_arr: StrArray): StrArray;
var
  insertion_sort_a: array of string;
  insertion_sort_i: int64;
  insertion_sort_key: string;
  insertion_sort_j: int64;
begin
  insertion_sort_a := insertion_sort_arr;
  insertion_sort_i := 1;
  while insertion_sort_i < Length(insertion_sort_a) do begin
  insertion_sort_key := insertion_sort_a[insertion_sort_i];
  insertion_sort_j := insertion_sort_i - 1;
  while (insertion_sort_j >= 0) and (insertion_sort_a[insertion_sort_j] > insertion_sort_key) do begin
  insertion_sort_a[insertion_sort_j + 1] := insertion_sort_a[insertion_sort_j];
  insertion_sort_j := insertion_sort_j - 1;
end;
  insertion_sort_a[insertion_sort_j + 1] := insertion_sort_key;
  insertion_sort_i := insertion_sort_i + 1;
end;
  exit(insertion_sort_a);
end;
function letter_value(letter_value_ch: string): int64;
var
  letter_value_alphabet: string;
  letter_value_idx: int64;
begin
  letter_value_alphabet := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  letter_value_idx := 0;
  while letter_value_idx < Length(letter_value_alphabet) do begin
  if copy(letter_value_alphabet, letter_value_idx+1, (letter_value_idx + 1 - (letter_value_idx))) = letter_value_ch then begin
  exit(letter_value_idx + 1);
end;
  letter_value_idx := letter_value_idx + 1;
end;
  exit(0);
end;
function name_score(name_score_name: string): int64;
var
  name_score_score: int64;
  name_score_i: int64;
begin
  name_score_score := 0;
  name_score_i := 0;
  while name_score_i < Length(name_score_name) do begin
  name_score_score := name_score_score + letter_value(copy(name_score_name, name_score_i+1, (name_score_i + 1 - (name_score_i))));
  name_score_i := name_score_i + 1;
end;
  exit(name_score_score);
end;
procedure main();
var
  main_line: string;
  main_names: StrArray;
  main_total: int64;
  main_i: int64;
begin
  main_line := _input();
  main_names := insertion_sort(parse_names(main_line));
  main_total := 0;
  main_i := 0;
  while main_i < Length(main_names) do begin
  main_total := main_total + ((main_i + 1) * name_score(main_names[main_i]));
  main_i := main_i + 1;
end;
  writeln(IntToStr(main_total));
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

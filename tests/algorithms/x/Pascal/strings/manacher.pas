{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function palindromic_string(palindromic_string_input_string: string): string; forward;
procedure main(); forward;
function palindromic_string(palindromic_string_input_string: string): string;
var
  palindromic_string_max_length: integer;
  palindromic_string_new_input_string: string;
  palindromic_string_output_string: string;
  palindromic_string_n: integer;
  palindromic_string_i: integer;
  palindromic_string_left: integer;
  palindromic_string_right: integer;
  palindromic_string_length_: array of integer;
  palindromic_string_m: integer;
  palindromic_string_start: integer;
  palindromic_string_j: integer;
  palindromic_string_k: integer;
  palindromic_string_mirror: integer;
  palindromic_string_diff: integer;
  palindromic_string_s: string;
  palindromic_string_idx: integer;
  palindromic_string_ch: string;
begin
  palindromic_string_max_length := 0;
  palindromic_string_new_input_string := '';
  palindromic_string_output_string := '';
  palindromic_string_n := Length(palindromic_string_input_string);
  palindromic_string_i := 0;
  while palindromic_string_i < (palindromic_string_n - 1) do begin
  palindromic_string_new_input_string := (palindromic_string_new_input_string + copy(palindromic_string_input_string, palindromic_string_i+1, (palindromic_string_i + 1 - (palindromic_string_i)))) + '|';
  palindromic_string_i := palindromic_string_i + 1;
end;
  palindromic_string_new_input_string := palindromic_string_new_input_string + copy(palindromic_string_input_string, palindromic_string_n - 1+1, (palindromic_string_n - (palindromic_string_n - 1)));
  palindromic_string_left := 0;
  palindromic_string_right := 0;
  palindromic_string_length_ := [];
  palindromic_string_i := 0;
  palindromic_string_m := Length(palindromic_string_new_input_string);
  while palindromic_string_i < palindromic_string_m do begin
  palindromic_string_length_ := concat(palindromic_string_length_, [1]);
  palindromic_string_i := palindromic_string_i + 1;
end;
  palindromic_string_start := 0;
  palindromic_string_j := 0;
  while palindromic_string_j < palindromic_string_m do begin
  palindromic_string_k := 1;
  if palindromic_string_j <= palindromic_string_right then begin
  palindromic_string_mirror := (palindromic_string_left + palindromic_string_right) - palindromic_string_j;
  palindromic_string_k := palindromic_string_length_[palindromic_string_mirror] div 2;
  palindromic_string_diff := (palindromic_string_right - palindromic_string_j) + 1;
  if palindromic_string_diff < palindromic_string_k then begin
  palindromic_string_k := palindromic_string_diff;
end;
  if palindromic_string_k < 1 then begin
  palindromic_string_k := 1;
end;
end;
  while (((palindromic_string_j - palindromic_string_k) >= 0) and ((palindromic_string_j + palindromic_string_k) < palindromic_string_m)) and (copy(palindromic_string_new_input_string, palindromic_string_j + palindromic_string_k+1, ((palindromic_string_j + palindromic_string_k) + 1 - (palindromic_string_j + palindromic_string_k))) = copy(palindromic_string_new_input_string, palindromic_string_j - palindromic_string_k+1, ((palindromic_string_j - palindromic_string_k) + 1 - (palindromic_string_j - palindromic_string_k)))) do begin
  palindromic_string_k := palindromic_string_k + 1;
end;
  palindromic_string_length_[palindromic_string_j] := (2 * palindromic_string_k) - 1;
  if ((palindromic_string_j + palindromic_string_k) - 1) > palindromic_string_right then begin
  palindromic_string_left := (palindromic_string_j - palindromic_string_k) + 1;
  palindromic_string_right := (palindromic_string_j + palindromic_string_k) - 1;
end;
  if palindromic_string_length_[palindromic_string_j] > palindromic_string_max_length then begin
  palindromic_string_max_length := palindromic_string_length_[palindromic_string_j];
  palindromic_string_start := palindromic_string_j;
end;
  palindromic_string_j := palindromic_string_j + 1;
end;
  palindromic_string_s := copy(palindromic_string_new_input_string, palindromic_string_start - (palindromic_string_max_length div 2)+1, ((palindromic_string_start + (palindromic_string_max_length div 2)) + 1 - (palindromic_string_start - (palindromic_string_max_length div 2))));
  palindromic_string_idx := 0;
  while palindromic_string_idx < Length(palindromic_string_s) do begin
  palindromic_string_ch := copy(palindromic_string_s, palindromic_string_idx+1, (palindromic_string_idx + 1 - (palindromic_string_idx)));
  if palindromic_string_ch <> '|' then begin
  palindromic_string_output_string := palindromic_string_output_string + palindromic_string_ch;
end;
  palindromic_string_idx := palindromic_string_idx + 1;
end;
  exit(palindromic_string_output_string);
end;
procedure main();
begin
  writeln(palindromic_string('abbbaba'));
  writeln(palindromic_string('ababa'));
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

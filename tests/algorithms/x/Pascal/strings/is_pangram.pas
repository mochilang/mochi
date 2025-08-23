{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
type BoolArray = array of boolean;
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
function contains(xs: array of int64; v: int64): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
function contains(xs: array of string; v: string): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  s1: string;
  s2: string;
function is_pangram(is_pangram_input_str: string): boolean; forward;
function is_pangram_faster(is_pangram_faster_input_str: string): boolean; forward;
function is_pangram_fastest(is_pangram_fastest_input_str: string): boolean; forward;
function is_pangram(is_pangram_input_str: string): boolean;
var
  is_pangram_letters: array of string;
  is_pangram_i: integer;
  is_pangram_c: string;
  is_pangram_is_new: boolean;
begin
  is_pangram_letters := [];
  is_pangram_i := 0;
  while is_pangram_i < Length(is_pangram_input_str) do begin
  is_pangram_c := LowerCase(is_pangram_input_str[is_pangram_i+1]);
  is_pangram_is_new := not (contains(is_pangram_letters, is_pangram_c));
  if (((is_pangram_c <> ' ') and ('a' <= is_pangram_c)) and (is_pangram_c <= 'z')) and is_pangram_is_new then begin
  is_pangram_letters := concat(is_pangram_letters, StrArray([is_pangram_c]));
end;
  is_pangram_i := is_pangram_i + 1;
end;
  exit(Length(is_pangram_letters) = 26);
end;
function is_pangram_faster(is_pangram_faster_input_str: string): boolean;
var
  is_pangram_faster_alphabet: string;
  is_pangram_faster_flag: array of boolean;
  is_pangram_faster_i: integer;
  is_pangram_faster_j: integer;
  is_pangram_faster_c: string;
  is_pangram_faster_k: integer;
  is_pangram_faster_t: integer;
begin
  is_pangram_faster_alphabet := 'abcdefghijklmnopqrstuvwxyz';
  is_pangram_faster_flag := [];
  is_pangram_faster_i := 0;
  while is_pangram_faster_i < 26 do begin
  is_pangram_faster_flag := concat(is_pangram_faster_flag, [false]);
  is_pangram_faster_i := is_pangram_faster_i + 1;
end;
  is_pangram_faster_j := 0;
  while is_pangram_faster_j < Length(is_pangram_faster_input_str) do begin
  is_pangram_faster_c := LowerCase(is_pangram_faster_input_str[is_pangram_faster_j+1]);
  is_pangram_faster_k := 0;
  while is_pangram_faster_k < 26 do begin
  if is_pangram_faster_alphabet[is_pangram_faster_k+1] = is_pangram_faster_c then begin
  is_pangram_faster_flag[is_pangram_faster_k] := true;
  break;
end;
  is_pangram_faster_k := is_pangram_faster_k + 1;
end;
  is_pangram_faster_j := is_pangram_faster_j + 1;
end;
  is_pangram_faster_t := 0;
  while is_pangram_faster_t < 26 do begin
  if not is_pangram_faster_flag[is_pangram_faster_t] then begin
  exit(false);
end;
  is_pangram_faster_t := is_pangram_faster_t + 1;
end;
  exit(true);
end;
function is_pangram_fastest(is_pangram_fastest_input_str: string): boolean;
var
  is_pangram_fastest_s: string;
  is_pangram_fastest_alphabet: string;
  is_pangram_fastest_i: integer;
  is_pangram_fastest_letter: string;
begin
  is_pangram_fastest_s := LowerCase(is_pangram_fastest_input_str);
  is_pangram_fastest_alphabet := 'abcdefghijklmnopqrstuvwxyz';
  is_pangram_fastest_i := 0;
  while is_pangram_fastest_i < Length(is_pangram_fastest_alphabet) do begin
  is_pangram_fastest_letter := is_pangram_fastest_alphabet[is_pangram_fastest_i+1];
  if not (Pos(is_pangram_fastest_letter, is_pangram_fastest_s) <> 0) then begin
  exit(false);
end;
  is_pangram_fastest_i := is_pangram_fastest_i + 1;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  s1 := 'The quick brown fox jumps over the lazy dog';
  s2 := 'My name is Unknown';
  writeln(LowerCase(BoolToStr(is_pangram(s1), true)));
  writeln(LowerCase(BoolToStr(is_pangram(s2), true)));
  writeln(LowerCase(BoolToStr(is_pangram_faster(s1), true)));
  writeln(LowerCase(BoolToStr(is_pangram_faster(s2), true)));
  writeln(LowerCase(BoolToStr(is_pangram_fastest(s1), true)));
  writeln(LowerCase(BoolToStr(is_pangram_fastest(s2), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

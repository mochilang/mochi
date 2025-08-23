{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
  check_anagrams_ch_idx: integer;
function strip_and_remove_spaces(strip_and_remove_spaces_s: string): string; forward;
function check_anagrams(check_anagrams_a: string; check_anagrams_b: string): boolean; forward;
procedure print_bool(print_bool_b: boolean); forward;
function strip_and_remove_spaces(strip_and_remove_spaces_s: string): string;
var
  strip_and_remove_spaces_start: integer;
  strip_and_remove_spaces_end_: integer;
  strip_and_remove_spaces_res: string;
  strip_and_remove_spaces_i: integer;
  strip_and_remove_spaces_ch: string;
begin
  strip_and_remove_spaces_start := 0;
  strip_and_remove_spaces_end_ := Length(strip_and_remove_spaces_s) - 1;
  while (strip_and_remove_spaces_start < Length(strip_and_remove_spaces_s)) and (strip_and_remove_spaces_s[strip_and_remove_spaces_start+1] = ' ') do begin
  strip_and_remove_spaces_start := strip_and_remove_spaces_start + 1;
end;
  while (strip_and_remove_spaces_end_ >= strip_and_remove_spaces_start) and (strip_and_remove_spaces_s[strip_and_remove_spaces_end_+1] = ' ') do begin
  strip_and_remove_spaces_end_ := strip_and_remove_spaces_end_ - 1;
end;
  strip_and_remove_spaces_res := '';
  strip_and_remove_spaces_i := strip_and_remove_spaces_start;
  while strip_and_remove_spaces_i <= strip_and_remove_spaces_end_ do begin
  strip_and_remove_spaces_ch := strip_and_remove_spaces_s[strip_and_remove_spaces_i+1];
  if strip_and_remove_spaces_ch <> ' ' then begin
  strip_and_remove_spaces_res := strip_and_remove_spaces_res + strip_and_remove_spaces_ch;
end;
  strip_and_remove_spaces_i := strip_and_remove_spaces_i + 1;
end;
  exit(strip_and_remove_spaces_res);
end;
function check_anagrams(check_anagrams_a: string; check_anagrams_b: string): boolean;
var
  check_anagrams_s1: string;
  check_anagrams_s2: string;
  check_anagrams_count: specialize TFPGMap<string, integer>;
  check_anagrams_i: integer;
  check_anagrams_c1: string;
  check_anagrams_c2: string;
  check_anagrams_ch: string;
begin
  check_anagrams_s1 := LowerCase(check_anagrams_a);
  check_anagrams_s2 := LowerCase(check_anagrams_b);
  check_anagrams_s1 := strip_and_remove_spaces(check_anagrams_s1);
  check_anagrams_s2 := strip_and_remove_spaces(check_anagrams_s2);
  if Length(check_anagrams_s1) <> Length(check_anagrams_s2) then begin
  exit(false);
end;
  check_anagrams_count := specialize TFPGMap<string, integer>.Create();
  check_anagrams_i := 0;
  while check_anagrams_i < Length(check_anagrams_s1) do begin
  check_anagrams_c1 := check_anagrams_s1[check_anagrams_i+1];
  check_anagrams_c2 := check_anagrams_s2[check_anagrams_i+1];
  if check_anagrams_count.IndexOf(check_anagrams_c1) <> -1 then begin
  check_anagrams_count[check_anagrams_c1] := check_anagrams_count[check_anagrams_c1] + 1;
end else begin
  check_anagrams_count[check_anagrams_c1] := 1;
end;
  if check_anagrams_count.IndexOf(check_anagrams_c2) <> -1 then begin
  check_anagrams_count[check_anagrams_c2] := check_anagrams_count[check_anagrams_c2] - 1;
end else begin
  check_anagrams_count[check_anagrams_c2] := -1;
end;
  check_anagrams_i := check_anagrams_i + 1;
end;
  for check_anagrams_ch_idx := 0 to (check_anagrams_count.Count - 1) do begin
  check_anagrams_ch := check_anagrams_count.Keys[check_anagrams_ch_idx];
  if check_anagrams_count[check_anagrams_ch] <> 0 then begin
  exit(false);
end;
end;
  exit(true);
end;
procedure print_bool(print_bool_b: boolean);
begin
  if print_bool_b then begin
  writeln(Ord(true));
end else begin
  writeln(Ord(false));
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  print_bool(check_anagrams('Silent', 'Listen'));
  print_bool(check_anagrams('This is a string', 'Is this a string'));
  print_bool(check_anagrams('This is    a      string', 'Is     this a string'));
  print_bool(check_anagrams('There', 'Their'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

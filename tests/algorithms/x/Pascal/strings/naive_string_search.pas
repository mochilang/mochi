{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
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
procedure show_list_int64(xs: array of int64);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function naive_string_search(naive_string_search_text: string; naive_string_search_pattern: string): IntArray; forward;
function naive_string_search(naive_string_search_text: string; naive_string_search_pattern: string): IntArray;
var
  naive_string_search_pat_len: integer;
  naive_string_search_positions: array of int64;
  naive_string_search_i: int64;
  naive_string_search_match_found: boolean;
  naive_string_search_j: int64;
begin
  naive_string_search_pat_len := Length(naive_string_search_pattern);
  naive_string_search_positions := [];
  naive_string_search_i := 0;
  while naive_string_search_i <= (Length(naive_string_search_text) - naive_string_search_pat_len) do begin
  naive_string_search_match_found := true;
  naive_string_search_j := 0;
  while naive_string_search_j < naive_string_search_pat_len do begin
  if naive_string_search_text[naive_string_search_i + naive_string_search_j+1] <> naive_string_search_pattern[naive_string_search_j+1] then begin
  naive_string_search_match_found := false;
  break;
end;
  naive_string_search_j := naive_string_search_j + 1;
end;
  if naive_string_search_match_found then begin
  naive_string_search_positions := concat(naive_string_search_positions, IntArray([naive_string_search_i]));
end;
  naive_string_search_i := naive_string_search_i + 1;
end;
  exit(naive_string_search_positions);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_list_int64(naive_string_search('ABAAABCDBBABCDDEBCABC', 'ABC'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

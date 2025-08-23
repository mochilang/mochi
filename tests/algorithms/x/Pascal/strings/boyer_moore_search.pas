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
function match_in_pattern(match_in_pattern_pat: string; match_in_pattern_ch: string): integer; forward;
function mismatch_in_text(mismatch_in_text_text: string; mismatch_in_text_pat: string; mismatch_in_text_current_pos: integer): integer; forward;
function bad_character_heuristic(bad_character_heuristic_text: string; bad_character_heuristic_pat: string): IntArray; forward;
function match_in_pattern(match_in_pattern_pat: string; match_in_pattern_ch: string): integer;
var
  match_in_pattern_i: integer;
begin
  match_in_pattern_i := Length(match_in_pattern_pat) - 1;
  while match_in_pattern_i >= 0 do begin
  if copy(match_in_pattern_pat, match_in_pattern_i+1, (match_in_pattern_i + 1 - (match_in_pattern_i))) = match_in_pattern_ch then begin
  exit(match_in_pattern_i);
end;
  match_in_pattern_i := match_in_pattern_i - 1;
end;
  exit(-1);
end;
function mismatch_in_text(mismatch_in_text_text: string; mismatch_in_text_pat: string; mismatch_in_text_current_pos: integer): integer;
var
  mismatch_in_text_i: integer;
begin
  mismatch_in_text_i := Length(mismatch_in_text_pat) - 1;
  while mismatch_in_text_i >= 0 do begin
  if copy(mismatch_in_text_pat, mismatch_in_text_i+1, (mismatch_in_text_i + 1 - (mismatch_in_text_i))) <> copy(mismatch_in_text_text, mismatch_in_text_current_pos + mismatch_in_text_i+1, ((mismatch_in_text_current_pos + mismatch_in_text_i) + 1 - (mismatch_in_text_current_pos + mismatch_in_text_i))) then begin
  exit(mismatch_in_text_current_pos + mismatch_in_text_i);
end;
  mismatch_in_text_i := mismatch_in_text_i - 1;
end;
  exit(-1);
end;
function bad_character_heuristic(bad_character_heuristic_text: string; bad_character_heuristic_pat: string): IntArray;
var
  bad_character_heuristic_textLen: integer;
  bad_character_heuristic_patLen: integer;
  bad_character_heuristic_positions: array of integer;
  bad_character_heuristic_i: integer;
  bad_character_heuristic_mismatch_index: integer;
  bad_character_heuristic_ch: string;
  bad_character_heuristic_match_index: integer;
begin
  bad_character_heuristic_textLen := Length(bad_character_heuristic_text);
  bad_character_heuristic_patLen := Length(bad_character_heuristic_pat);
  bad_character_heuristic_positions := [];
  bad_character_heuristic_i := 0;
  while bad_character_heuristic_i <= (bad_character_heuristic_textLen - bad_character_heuristic_patLen) do begin
  bad_character_heuristic_mismatch_index := mismatch_in_text(bad_character_heuristic_text, bad_character_heuristic_pat, bad_character_heuristic_i);
  if bad_character_heuristic_mismatch_index < 0 then begin
  bad_character_heuristic_positions := concat(bad_character_heuristic_positions, [bad_character_heuristic_i]);
  bad_character_heuristic_i := bad_character_heuristic_i + 1;
end else begin
  bad_character_heuristic_ch := copy(bad_character_heuristic_text, bad_character_heuristic_mismatch_index+1, (bad_character_heuristic_mismatch_index + 1 - (bad_character_heuristic_mismatch_index)));
  bad_character_heuristic_match_index := match_in_pattern(bad_character_heuristic_pat, bad_character_heuristic_ch);
  if bad_character_heuristic_match_index < 0 then begin
  bad_character_heuristic_i := bad_character_heuristic_mismatch_index + 1;
end else begin
  bad_character_heuristic_i := bad_character_heuristic_mismatch_index - bad_character_heuristic_match_index;
end;
end;
end;
  exit(bad_character_heuristic_positions);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
type IntArrayArray = array of IntArray;
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
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
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
  query: string;
  subject: string;
  score: IntArrayArray;
function score_function(score_function_source_char: string; score_function_target_char: string; score_function_match_score: int64; score_function_mismatch_score: int64; score_function_gap_score: int64): int64; forward;
function smith_waterman(smith_waterman_query: string; smith_waterman_subject: string; smith_waterman_match_score: int64; smith_waterman_mismatch_score: int64; smith_waterman_gap_score: int64): IntArrayArray; forward;
function traceback(traceback_score: IntArrayArray; traceback_query: string; traceback_subject: string; traceback_match_score: int64; traceback_mismatch_score: int64; traceback_gap_score: int64): string; forward;
function score_function(score_function_source_char: string; score_function_target_char: string; score_function_match_score: int64; score_function_mismatch_score: int64; score_function_gap_score: int64): int64;
begin
  if (score_function_source_char = '-') or (score_function_target_char = '-') then begin
  exit(score_function_gap_score);
end;
  if score_function_source_char = score_function_target_char then begin
  exit(score_function_match_score);
end;
  exit(score_function_mismatch_score);
end;
function smith_waterman(smith_waterman_query: string; smith_waterman_subject: string; smith_waterman_match_score: int64; smith_waterman_mismatch_score: int64; smith_waterman_gap_score: int64): IntArrayArray;
var
  smith_waterman_q: string;
  smith_waterman_s: string;
  smith_waterman_m: integer;
  smith_waterman_n: integer;
  smith_waterman_score: array of IntArray;
  smith_waterman__: int64;
  smith_waterman_row: array of int64;
  smith_waterman__2: int64;
  smith_waterman_i: int64;
  smith_waterman_j: int64;
  smith_waterman_qc: string;
  smith_waterman_sc: string;
  smith_waterman_diag: int64;
  smith_waterman_delete: int64;
  smith_waterman_insert: int64;
  smith_waterman_max_val: int64;
begin
  smith_waterman_q := UpperCase(smith_waterman_query);
  smith_waterman_s := UpperCase(smith_waterman_subject);
  smith_waterman_m := Length(smith_waterman_q);
  smith_waterman_n := Length(smith_waterman_s);
  smith_waterman_score := [];
  for smith_waterman__ := 0 to (smith_waterman_m + 1 - 1) do begin
  smith_waterman_row := [];
  for smith_waterman__2 := 0 to (smith_waterman_n + 1 - 1) do begin
  smith_waterman_row := concat(smith_waterman_row, IntArray([0]));
end;
  smith_waterman_score := concat(smith_waterman_score, [smith_waterman_row]);
end;
  for smith_waterman_i := 1 to (smith_waterman_m + 1 - 1) do begin
  for smith_waterman_j := 1 to (smith_waterman_n + 1 - 1) do begin
  smith_waterman_qc := copy(smith_waterman_q, smith_waterman_i - 1+1, (smith_waterman_i - (smith_waterman_i - 1)));
  smith_waterman_sc := copy(smith_waterman_s, smith_waterman_j - 1+1, (smith_waterman_j - (smith_waterman_j - 1)));
  smith_waterman_diag := smith_waterman_score[smith_waterman_i - 1][smith_waterman_j - 1] + score_function(smith_waterman_qc, smith_waterman_sc, smith_waterman_match_score, smith_waterman_mismatch_score, smith_waterman_gap_score);
  smith_waterman_delete := smith_waterman_score[smith_waterman_i - 1][smith_waterman_j] + smith_waterman_gap_score;
  smith_waterman_insert := smith_waterman_score[smith_waterman_i][smith_waterman_j - 1] + smith_waterman_gap_score;
  smith_waterman_max_val := 0;
  if smith_waterman_diag > smith_waterman_max_val then begin
  smith_waterman_max_val := smith_waterman_diag;
end;
  if smith_waterman_delete > smith_waterman_max_val then begin
  smith_waterman_max_val := smith_waterman_delete;
end;
  if smith_waterman_insert > smith_waterman_max_val then begin
  smith_waterman_max_val := smith_waterman_insert;
end;
  smith_waterman_score[smith_waterman_i][smith_waterman_j] := smith_waterman_max_val;
end;
end;
  exit(smith_waterman_score);
end;
function traceback(traceback_score: IntArrayArray; traceback_query: string; traceback_subject: string; traceback_match_score: int64; traceback_mismatch_score: int64; traceback_gap_score: int64): string;
var
  traceback_q: string;
  traceback_s: string;
  traceback_max_value: int64;
  traceback_i_max: int64;
  traceback_j_max: int64;
  traceback_i: int64;
  traceback_j: int64;
  traceback_align1: string;
  traceback_align2: string;
  traceback_gap_penalty: int64;
  traceback_qc: string;
  traceback_sc: string;
begin
  traceback_q := UpperCase(traceback_query);
  traceback_s := UpperCase(traceback_subject);
  traceback_max_value := 0;
  traceback_i_max := 0;
  traceback_j_max := 0;
  for traceback_i := 0 to (Length(traceback_score) - 1) do begin
  for traceback_j := 0 to (Length(traceback_score[traceback_i]) - 1) do begin
  if traceback_score[traceback_i][traceback_j] > traceback_max_value then begin
  traceback_max_value := traceback_score[traceback_i][traceback_j];
  traceback_i_max := traceback_i;
  traceback_j_max := traceback_j;
end;
end;
end;
  traceback_i := traceback_i_max;
  traceback_j := traceback_j_max;
  traceback_align1 := '';
  traceback_align2 := '';
  traceback_gap_penalty := score_function('-', '-', traceback_match_score, traceback_mismatch_score, traceback_gap_score);
  if (traceback_i = 0) or (traceback_j = 0) then begin
  exit('');
end;
  while (traceback_i > 0) and (traceback_j > 0) do begin
  traceback_qc := copy(traceback_q, traceback_i - 1+1, (traceback_i - (traceback_i - 1)));
  traceback_sc := copy(traceback_s, traceback_j - 1+1, (traceback_j - (traceback_j - 1)));
  if traceback_score[traceback_i][traceback_j] = (traceback_score[traceback_i - 1][traceback_j - 1] + score_function(traceback_qc, traceback_sc, traceback_match_score, traceback_mismatch_score, traceback_gap_score)) then begin
  traceback_align1 := traceback_qc + traceback_align1;
  traceback_align2 := traceback_sc + traceback_align2;
  traceback_i := traceback_i - 1;
  traceback_j := traceback_j - 1;
end else begin
  if traceback_score[traceback_i][traceback_j] = (traceback_score[traceback_i - 1][traceback_j] + traceback_gap_penalty) then begin
  traceback_align1 := traceback_qc + traceback_align1;
  traceback_align2 := '-' + traceback_align2;
  traceback_i := traceback_i - 1;
end else begin
  traceback_align1 := '-' + traceback_align1;
  traceback_align2 := traceback_sc + traceback_align2;
  traceback_j := traceback_j - 1;
end;
end;
end;
  exit((traceback_align1 + #10) + traceback_align2);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  query := 'HEAGAWGHEE';
  subject := 'PAWHEAE';
  score := smith_waterman(query, subject, 1, -1, -2);
  writeln(traceback(score, query, subject, 1, -1, -2));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

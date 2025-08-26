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
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
function list_int_to_str(xs: array of int64): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function index_of(index_of_xs: IntArray; index_of_x: int64): int64; forward;
function majority_vote(majority_vote_votes: IntArray; majority_vote_votes_needed_to_win: int64): IntArray; forward;
procedure main(); forward;
function index_of(index_of_xs: IntArray; index_of_x: int64): int64;
var
  index_of_i: int64;
begin
  index_of_i := 0;
  while index_of_i < Length(index_of_xs) do begin
  if index_of_xs[index_of_i] = index_of_x then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(0 - 1);
end;
function majority_vote(majority_vote_votes: IntArray; majority_vote_votes_needed_to_win: int64): IntArray;
var
  majority_vote_candidates: array of int64;
  majority_vote_counts: array of int64;
  majority_vote_i: int64;
  majority_vote_v: int64;
  majority_vote_idx: int64;
  majority_vote_j: int64;
  majority_vote_new_candidates: array of int64;
  majority_vote_new_counts: array of int64;
  majority_vote_final_counts: array of int64;
  majority_vote_j_14: int64;
  majority_vote_result_: array of int64;
begin
  if majority_vote_votes_needed_to_win < 2 then begin
  exit([]);
end;
  majority_vote_candidates := [];
  majority_vote_counts := [];
  majority_vote_i := 0;
  while majority_vote_i < Length(majority_vote_votes) do begin
  majority_vote_v := majority_vote_votes[majority_vote_i];
  majority_vote_idx := index_of(majority_vote_candidates, majority_vote_v);
  if majority_vote_idx <> (0 - 1) then begin
  majority_vote_counts[majority_vote_idx] := majority_vote_counts[majority_vote_idx] + 1;
end else begin
  if Length(majority_vote_candidates) < (majority_vote_votes_needed_to_win - 1) then begin
  majority_vote_candidates := concat(majority_vote_candidates, IntArray([majority_vote_v]));
  majority_vote_counts := concat(majority_vote_counts, IntArray([1]));
end else begin
  majority_vote_j := 0;
  while majority_vote_j < Length(majority_vote_counts) do begin
  majority_vote_counts[majority_vote_j] := majority_vote_counts[majority_vote_j] - 1;
  majority_vote_j := majority_vote_j + 1;
end;
  majority_vote_new_candidates := [];
  majority_vote_new_counts := [];
  majority_vote_j := 0;
  while majority_vote_j < Length(majority_vote_candidates) do begin
  if majority_vote_counts[majority_vote_j] > 0 then begin
  majority_vote_new_candidates := concat(majority_vote_new_candidates, IntArray([majority_vote_candidates[majority_vote_j]]));
  majority_vote_new_counts := concat(majority_vote_new_counts, IntArray([majority_vote_counts[majority_vote_j]]));
end;
  majority_vote_j := majority_vote_j + 1;
end;
  majority_vote_candidates := majority_vote_new_candidates;
  majority_vote_counts := majority_vote_new_counts;
end;
end;
  majority_vote_i := majority_vote_i + 1;
end;
  majority_vote_final_counts := [];
  majority_vote_j_14 := 0;
  while majority_vote_j_14 < Length(majority_vote_candidates) do begin
  majority_vote_final_counts := concat(majority_vote_final_counts, IntArray([0]));
  majority_vote_j_14 := majority_vote_j_14 + 1;
end;
  majority_vote_i := 0;
  while majority_vote_i < Length(majority_vote_votes) do begin
  majority_vote_v := majority_vote_votes[majority_vote_i];
  majority_vote_idx := index_of(majority_vote_candidates, majority_vote_v);
  if majority_vote_idx <> (0 - 1) then begin
  majority_vote_final_counts[majority_vote_idx] := majority_vote_final_counts[majority_vote_idx] + 1;
end;
  majority_vote_i := majority_vote_i + 1;
end;
  majority_vote_result_ := [];
  majority_vote_j_14 := 0;
  while majority_vote_j_14 < Length(majority_vote_candidates) do begin
  if (majority_vote_final_counts[majority_vote_j_14] * majority_vote_votes_needed_to_win) > Length(majority_vote_votes) then begin
  majority_vote_result_ := concat(majority_vote_result_, IntArray([majority_vote_candidates[majority_vote_j_14]]));
end;
  majority_vote_j_14 := majority_vote_j_14 + 1;
end;
  exit(majority_vote_result_);
end;
procedure main();
var
  main_votes: array of int64;
begin
  main_votes := [1, 2, 2, 3, 1, 3, 2];
  writeln(list_int_to_str(majority_vote(main_votes, 3)));
  writeln(list_int_to_str(majority_vote(main_votes, 2)));
  writeln(list_int_to_str(majority_vote(main_votes, 4)));
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

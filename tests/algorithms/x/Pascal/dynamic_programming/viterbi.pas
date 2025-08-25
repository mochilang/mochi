{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
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
  observations: array of string;
  states: array of string;
  start_p: specialize TFPGMap<string, real>;
  trans_p: specialize TFPGMap<string, specialize TFPGMap<string, real>>;
  emit_p: specialize TFPGMap<string, specialize TFPGMap<string, real>>;
  result_: StrArray;
function Map6(): specialize TFPGMap<string, real>; forward;
function Map5(): specialize TFPGMap<string, real>; forward;
function Map4(): specialize TFPGMap<string, specialize TFPGMap<string, real>>; forward;
function Map3(): specialize TFPGMap<string, real>; forward;
function Map2(): specialize TFPGMap<string, real>; forward;
function Map1(): specialize TFPGMap<string, real>; forward;
function key(key_state: string; key_obs: string): string; forward;
function viterbi(viterbi_observations: StrArray; viterbi_states: StrArray; viterbi_start_p: specialize TFPGMap<string, real>; viterbi_trans_p: specialize TFPGMap<string, specialize TFPGMap<string, real>>; viterbi_emit_p: specialize TFPGMap<string, specialize TFPGMap<string, real>>): StrArray; forward;
function join_words(join_words_words: StrArray): string; forward;
function Map6(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('normal', 0.1);
  Result.AddOrSetData('cold', 0.3);
  Result.AddOrSetData('dizzy', 0.6);
end;
function Map5(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('normal', 0.5);
  Result.AddOrSetData('cold', 0.4);
  Result.AddOrSetData('dizzy', 0.1);
end;
function Map4(): specialize TFPGMap<string, specialize TFPGMap<string, real>>;
begin
  Result := specialize TFPGMap<string, specialize TFPGMap<string, real>>.Create();
  Result.AddOrSetData('Healthy', Map2());
  Result.AddOrSetData('Fever', Map3());
end;
function Map3(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('Healthy', 0.4);
  Result.AddOrSetData('Fever', 0.6);
end;
function Map2(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('Healthy', 0.7);
  Result.AddOrSetData('Fever', 0.3);
end;
function Map1(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('Healthy', 0.6);
  Result.AddOrSetData('Fever', 0.4);
end;
function key(key_state: string; key_obs: string): string;
begin
  exit((key_state + '|') + key_obs);
end;
function viterbi(viterbi_observations: StrArray; viterbi_states: StrArray; viterbi_start_p: specialize TFPGMap<string, real>; viterbi_trans_p: specialize TFPGMap<string, specialize TFPGMap<string, real>>; viterbi_emit_p: specialize TFPGMap<string, specialize TFPGMap<string, real>>): StrArray;
var
  viterbi_probs: specialize TFPGMap<string, real>;
  viterbi_ptrs: specialize TFPGMap<string, string>;
  viterbi_first_obs: string;
  viterbi_i: int64;
  viterbi_state: string;
  viterbi_t: int64;
  viterbi_obs: string;
  viterbi_j: int64;
  viterbi_max_prob: real;
  viterbi_prev_state: string;
  viterbi_k: int64;
  viterbi_state0: string;
  viterbi_obs0: string;
  viterbi_prob_prev: real;
  viterbi_prob_prev_idx: integer;
  viterbi_prob: real;
  viterbi_path: array of string;
  viterbi_n: int64;
  viterbi_last_obs: string;
  viterbi_max_final: real;
  viterbi_last_state: string;
  viterbi_m: int64;
  viterbi_prob_idx: integer;
  viterbi_last_index: int64;
  viterbi_idx: int64;
  viterbi_prev: string;
  viterbi_prev_idx: integer;
begin
  if (Length(viterbi_observations) = 0) or (Length(viterbi_states) = 0) then begin
  panic('empty parameters');
end;
  viterbi_probs := specialize TFPGMap<string, real>.Create();
  viterbi_ptrs := specialize TFPGMap<string, string>.Create();
  viterbi_first_obs := viterbi_observations[0];
  viterbi_i := 0;
  while viterbi_i < Length(viterbi_states) do begin
  viterbi_state := viterbi_states[viterbi_i];
  viterbi_probs[key(viterbi_state, viterbi_first_obs)] := viterbi_start_p[viterbi_state] * viterbi_emit_p[viterbi_state][viterbi_first_obs];
  viterbi_ptrs[key(viterbi_state, viterbi_first_obs)] := '';
  viterbi_i := viterbi_i + 1;
end;
  viterbi_t := 1;
  while viterbi_t < Length(viterbi_observations) do begin
  viterbi_obs := viterbi_observations[viterbi_t];
  viterbi_j := 0;
  while viterbi_j < Length(viterbi_states) do begin
  viterbi_state := viterbi_states[viterbi_j];
  viterbi_max_prob := -1;
  viterbi_prev_state := '';
  viterbi_k := 0;
  while viterbi_k < Length(viterbi_states) do begin
  viterbi_state0 := viterbi_states[viterbi_k];
  viterbi_obs0 := viterbi_observations[viterbi_t - 1];
  viterbi_prob_prev_idx := viterbi_probs.IndexOf(key(viterbi_state0, viterbi_obs0));
  if viterbi_prob_prev_idx <> -1 then begin
  viterbi_prob_prev := viterbi_probs.Data[viterbi_prob_prev_idx];
end else begin
  viterbi_prob_prev := 0;
end;
  viterbi_prob := (viterbi_prob_prev * viterbi_trans_p[viterbi_state0][viterbi_state]) * viterbi_emit_p[viterbi_state][viterbi_obs];
  if viterbi_prob > viterbi_max_prob then begin
  viterbi_max_prob := viterbi_prob;
  viterbi_prev_state := viterbi_state0;
end;
  viterbi_k := viterbi_k + 1;
end;
  viterbi_probs[key(viterbi_state, viterbi_obs)] := viterbi_max_prob;
  viterbi_ptrs[key(viterbi_state, viterbi_obs)] := viterbi_prev_state;
  viterbi_j := viterbi_j + 1;
end;
  viterbi_t := viterbi_t + 1;
end;
  viterbi_path := [];
  viterbi_n := 0;
  while viterbi_n < Length(viterbi_observations) do begin
  viterbi_path := concat(viterbi_path, StrArray(['']));
  viterbi_n := viterbi_n + 1;
end;
  viterbi_last_obs := viterbi_observations[Length(viterbi_observations) - 1];
  viterbi_max_final := -1;
  viterbi_last_state := '';
  viterbi_m := 0;
  while viterbi_m < Length(viterbi_states) do begin
  viterbi_state := viterbi_states[viterbi_m];
  viterbi_prob_idx := viterbi_probs.IndexOf(key(viterbi_state, viterbi_last_obs));
  if viterbi_prob_idx <> -1 then begin
  viterbi_prob := viterbi_probs.Data[viterbi_prob_idx];
end else begin
  viterbi_prob := 0;
end;
  if viterbi_prob > viterbi_max_final then begin
  viterbi_max_final := viterbi_prob;
  viterbi_last_state := viterbi_state;
end;
  viterbi_m := viterbi_m + 1;
end;
  viterbi_last_index := Length(viterbi_observations) - 1;
  viterbi_path[viterbi_last_index] := viterbi_last_state;
  viterbi_idx := viterbi_last_index;
  while viterbi_idx > 0 do begin
  viterbi_obs := viterbi_observations[viterbi_idx];
  viterbi_prev_idx := viterbi_ptrs.IndexOf(key(viterbi_path[viterbi_idx], viterbi_obs));
  if viterbi_prev_idx <> -1 then begin
  viterbi_prev := viterbi_ptrs.Data[viterbi_prev_idx];
end else begin
  viterbi_prev := '';
end;
  viterbi_path[viterbi_idx - 1] := viterbi_prev;
  viterbi_idx := viterbi_idx - 1;
end;
  exit(viterbi_path);
end;
function join_words(join_words_words: StrArray): string;
var
  join_words_res: string;
  join_words_i: int64;
begin
  join_words_res := '';
  join_words_i := 0;
  while join_words_i < Length(join_words_words) do begin
  if join_words_i > 0 then begin
  join_words_res := join_words_res + ' ';
end;
  join_words_res := join_words_res + join_words_words[join_words_i];
  join_words_i := join_words_i + 1;
end;
  exit(join_words_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  observations := ['normal', 'cold', 'dizzy'];
  states := ['Healthy', 'Fever'];
  start_p := Map1();
  trans_p := Map4();
  emit_p := Map4();
  result_ := viterbi(observations, states, start_p, trans_p, emit_p);
  writeln(join_words(result_));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

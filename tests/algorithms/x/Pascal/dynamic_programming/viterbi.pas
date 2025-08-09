{$mode objfpc}
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
  obs: string;
  words: StrArray;
  state: string;
function Map3(): specialize TFPGMap<string, real>; forward;
function Map2(): specialize TFPGMap<string, Variant>; forward;
function Map1(): specialize TFPGMap<string, real>; forward;
function key(state: string; obs: string): string; forward;
function viterbi(observations: StrArray; states: StrArray; start_p: specialize TFPGMap<string, real>; trans_p: specialize TFPGMap<string, specialize TFPGMap<string, real>>; emit_p: specialize TFPGMap<string, specialize TFPGMap<string, real>>): StrArray; forward;
function join_words(words: StrArray): string; forward;
function Map3(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('normal', Variant(0.5));
  Result.AddOrSetData('cold', Variant(0.4));
  Result.AddOrSetData('dizzy', Variant(0.1));
end;
function Map2(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('Healthy', Variant(Map1()));
  Result.AddOrSetData('Fever', Variant(Map1()));
end;
function Map1(): specialize TFPGMap<string, real>;
begin
  Result := specialize TFPGMap<string, real>.Create();
  Result.AddOrSetData('Healthy', Variant(0.6));
  Result.AddOrSetData('Fever', Variant(0.4));
end;
function key(state: string; obs: string): string;
begin
  exit((state + '|') + obs);
end;
function viterbi(observations: StrArray; states: StrArray; start_p: specialize TFPGMap<string, real>; trans_p: specialize TFPGMap<string, specialize TFPGMap<string, real>>; emit_p: specialize TFPGMap<string, specialize TFPGMap<string, real>>): StrArray;
var
  viterbi_probs: specialize TFPGMap<string, real>;
  viterbi_ptrs: specialize TFPGMap<string, string>;
  viterbi_first_obs: string;
  viterbi_i: integer;
  viterbi_state: string;
  viterbi_t: integer;
  viterbi_obs: string;
  viterbi_j: integer;
  viterbi_max_prob: real;
  viterbi_prev_state: string;
  viterbi_k: integer;
  viterbi_state0: string;
  viterbi_obs0: string;
  viterbi_prob_prev: integer;
  viterbi_prob_prev_idx: integer;
  viterbi_prob: integer;
  viterbi_path: array of string;
  viterbi_n: integer;
  viterbi_last_obs: string;
  viterbi_max_final: real;
  viterbi_last_state: string;
  viterbi_m: integer;
  viterbi_prob_idx: integer;
  viterbi_last_index: integer;
  viterbi_idx: integer;
  viterbi_prev: integer;
  viterbi_prev_idx: integer;
begin
  if (Length(observations) = 0) or (Length(states) = 0) then begin
  panic('empty parameters');
end;
  viterbi_probs := specialize TFPGMap<string, real>.Create();
  viterbi_ptrs := specialize TFPGMap<string, string>.Create();
  viterbi_first_obs := observations[0];
  viterbi_i := 0;
  while viterbi_i < Length(states) do begin
  viterbi_state := states[viterbi_i];
  viterbi_probs[key(viterbi_state, viterbi_first_obs)] := start_p[viterbi_state] * emit_p[viterbi_state][viterbi_first_obs];
  viterbi_ptrs[key(viterbi_state, viterbi_first_obs)] := '';
  viterbi_i := viterbi_i + 1;
end;
  viterbi_t := 1;
  while viterbi_t < Length(observations) do begin
  viterbi_obs := observations[viterbi_t];
  viterbi_j := 0;
  while viterbi_j < Length(states) do begin
  viterbi_state := states[viterbi_j];
  viterbi_max_prob := -1;
  viterbi_prev_state := '';
  viterbi_k := 0;
  while viterbi_k < Length(states) do begin
  viterbi_state0 := states[viterbi_k];
  viterbi_obs0 := observations[viterbi_t - 1];
  viterbi_prob_prev_idx := viterbi_probs.IndexOf(key(viterbi_state0, viterbi_obs0));
  if viterbi_prob_prev_idx <> -1 then begin
  viterbi_prob_prev := viterbi_probs.Data[viterbi_prob_prev_idx];
end else begin
  viterbi_prob_prev := 0;
end;
  viterbi_prob := (viterbi_prob_prev * trans_p[viterbi_state0][viterbi_state]) * emit_p[viterbi_state][viterbi_obs];
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
  while viterbi_n < Length(observations) do begin
  viterbi_path := concat(viterbi_path, StrArray(['']));
  viterbi_n := viterbi_n + 1;
end;
  viterbi_last_obs := observations[Length(observations) - 1];
  viterbi_max_final := -1;
  viterbi_last_state := '';
  viterbi_m := 0;
  while viterbi_m < Length(states) do begin
  viterbi_state := states[viterbi_m];
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
  viterbi_last_index := Length(observations) - 1;
  viterbi_path[viterbi_last_index] := viterbi_last_state;
  viterbi_idx := viterbi_last_index;
  while viterbi_idx > 0 do begin
  viterbi_obs := observations[viterbi_idx];
  viterbi_prev_idx := viterbi_ptrs.IndexOf(key(viterbi_path[viterbi_idx], viterbi_obs));
  if viterbi_prev_idx <> -1 then begin
  viterbi_prev := viterbi_ptrs.Data[viterbi_prev_idx];
end else begin
  viterbi_prev := 0;
end;
  viterbi_path[viterbi_idx - 1] := viterbi_prev;
  viterbi_idx := viterbi_idx - 1;
end;
  exit(viterbi_path);
end;
function join_words(words: StrArray): string;
var
  join_words_res: string;
  join_words_i: integer;
begin
  join_words_res := '';
  join_words_i := 0;
  while join_words_i < Length(words) do begin
  if join_words_i > 0 then begin
  join_words_res := join_words_res + ' ';
end;
  join_words_res := join_words_res + words[join_words_i];
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
  trans_p := Map2();
  emit_p := Map2();
  result_ := viterbi(observations, states, start_p, trans_p, emit_p);
  writeln(join_words(result_));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

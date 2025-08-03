{$mode objfpc}
program Main;
uses SysUtils, Variants;
type VariantArray = array of Variant;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  seed: integer;
function nextRand(seed: integer): integer; forward;
function shuffleChars(s: string; seed: integer): VariantArray; forward;
function bestShuffle(s: string; seed: integer): VariantArray; forward;
procedure main(); forward;
function nextRand(seed: integer): integer;
begin
  exit(((seed * 1664525) + 1013904223) mod 2147483647);
end;
function shuffleChars(s: string; seed: integer): VariantArray;
var
  shuffleChars_chars: array of string;
  shuffleChars_i: integer;
  shuffleChars_sd: integer;
  shuffleChars_idx: integer;
  shuffleChars_j: integer;
  shuffleChars_tmp: string;
  shuffleChars_res: string;
begin
  shuffleChars_chars := [];
  shuffleChars_i := 0;
  while shuffleChars_i < Length(s) do begin
  shuffleChars_chars := concat(shuffleChars_chars, [copy(s, shuffleChars_i+1, (shuffleChars_i + 1 - (shuffleChars_i)))]);
  shuffleChars_i := shuffleChars_i + 1;
end;
  shuffleChars_sd := seed;
  shuffleChars_idx := Length(shuffleChars_chars) - 1;
  while shuffleChars_idx > 0 do begin
  shuffleChars_sd := nextRand(shuffleChars_sd);
  shuffleChars_j := shuffleChars_sd mod (shuffleChars_idx + 1);
  shuffleChars_tmp := shuffleChars_chars[shuffleChars_idx];
  shuffleChars_chars[shuffleChars_idx] := shuffleChars_chars[shuffleChars_j];
  shuffleChars_chars[shuffleChars_j] := shuffleChars_tmp;
  shuffleChars_idx := shuffleChars_idx - 1;
end;
  shuffleChars_res := '';
  shuffleChars_i := 0;
  while shuffleChars_i < Length(shuffleChars_chars) do begin
  shuffleChars_res := shuffleChars_res + shuffleChars_chars[shuffleChars_i];
  shuffleChars_i := shuffleChars_i + 1;
end;
  exit([shuffleChars_res, shuffleChars_sd]);
end;
function bestShuffle(s: string; seed: integer): VariantArray;
var
  bestShuffle_r: VariantArray;
  bestShuffle_t: Variant;
  bestShuffle_sd: Variant;
  bestShuffle_arr: array of string;
  bestShuffle_i: integer;
  bestShuffle_j: integer;
  bestShuffle_tmp: string;
  bestShuffle_count: integer;
  bestShuffle_out: string;
begin
  bestShuffle_r := shuffleChars(s, seed);
  bestShuffle_t := bestShuffle_r[0];
  bestShuffle_sd := bestShuffle_r[1];
  bestShuffle_arr := [];
  bestShuffle_i := 0;
  while bestShuffle_i < Length(bestShuffle_t) do begin
  bestShuffle_arr := concat(bestShuffle_arr, [copy(bestShuffle_t, bestShuffle_i+1, (bestShuffle_i + 1 - (bestShuffle_i)))]);
  bestShuffle_i := bestShuffle_i + 1;
end;
  bestShuffle_i := 0;
  while bestShuffle_i < Length(bestShuffle_arr) do begin
  bestShuffle_j := 0;
  while bestShuffle_j < Length(bestShuffle_arr) do begin
  if ((bestShuffle_i <> bestShuffle_j) and (bestShuffle_arr[bestShuffle_i] <> copy(s, bestShuffle_j+1, (bestShuffle_j + 1 - (bestShuffle_j))))) and (bestShuffle_arr[bestShuffle_j] <> copy(s, bestShuffle_i+1, (bestShuffle_i + 1 - (bestShuffle_i)))) then begin
  bestShuffle_tmp := bestShuffle_arr[bestShuffle_i];
  bestShuffle_arr[bestShuffle_i] := bestShuffle_arr[bestShuffle_j];
  bestShuffle_arr[bestShuffle_j] := bestShuffle_tmp;
  break;
end;
  bestShuffle_j := bestShuffle_j + 1;
end;
  bestShuffle_i := bestShuffle_i + 1;
end;
  bestShuffle_count := 0;
  bestShuffle_i := 0;
  while bestShuffle_i < Length(bestShuffle_arr) do begin
  if bestShuffle_arr[bestShuffle_i] = copy(s, bestShuffle_i+1, (bestShuffle_i + 1 - (bestShuffle_i))) then begin
  bestShuffle_count := bestShuffle_count + 1;
end;
  bestShuffle_i := bestShuffle_i + 1;
end;
  bestShuffle_out := '';
  bestShuffle_i := 0;
  while bestShuffle_i < Length(bestShuffle_arr) do begin
  bestShuffle_out := bestShuffle_out + bestShuffle_arr[bestShuffle_i];
  bestShuffle_i := bestShuffle_i + 1;
end;
  exit([bestShuffle_out, bestShuffle_sd, bestShuffle_count]);
end;
procedure main();
var
  main_ts: array of string;
  main_i: integer;
  main_r: VariantArray;
  main_shuf: Variant;
  main_cnt: Variant;
begin
  main_ts := ['abracadabra', 'seesaw', 'elk', 'grrrrrr', 'up', 'a'];
  seed := 1;
  main_i := 0;
  while main_i < Length(main_ts) do begin
  main_r := bestShuffle(main_ts[main_i], seed);
  main_shuf := main_r[0];
  seed := main_r[1];
  main_cnt := main_r[2];
  writeln(((((main_ts[main_i] + ' -> ') + main_shuf) + ' (') + VarToStr(main_cnt)) + ')');
  main_i := main_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

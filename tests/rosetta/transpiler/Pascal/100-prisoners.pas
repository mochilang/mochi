{$mode objfpc}
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
  shuffle_arr: array of integer;
  shuffle_i: integer;
  shuffle_j: integer;
  shuffle_tmp: integer;
  doTrials_pardoned: integer;
  doTrials_t: integer;
  doTrials_drawers: array of integer;
  doTrials_i: integer;
  doTrials_p: integer;
  doTrials_success: boolean;
  doTrials_found: boolean;
  doTrials_prev: integer;
  doTrials_d: integer;
  doTrials_this: integer;
  doTrials_opened: array of boolean;
  doTrials_k: integer;
  doTrials_n: integer;
  doTrials_rf: integer;
  main_trials: integer;
  np: integer;
  strat: string;
function shuffle(xs: IntArray): IntArray;
begin
  shuffle_arr := xs;
  shuffle_i := 99;
  while shuffle_i > 0 do begin
  shuffle_j := _now() mod (shuffle_i + 1);
  shuffle_tmp := shuffle_arr[shuffle_i];
  shuffle_arr[shuffle_i] := shuffle_arr[shuffle_j];
  shuffle_arr[shuffle_j] := shuffle_tmp;
  shuffle_i := shuffle_i - 1;
end;
  exit(shuffle_arr);
end;
procedure doTrials(trials: integer; np: integer; strategy: string);
begin
  doTrials_pardoned := 0;
  doTrials_t := 0;
  while doTrials_t < trials do begin
  doTrials_drawers := [];
  doTrials_i := 0;
  while doTrials_i < 100 do begin
  doTrials_drawers := concat(doTrials_drawers, [doTrials_i]);
  doTrials_i := doTrials_i + 1;
end;
  doTrials_drawers := shuffle(doTrials_drawers);
  doTrials_p := 0;
  doTrials_success := true;
  while doTrials_p < np do begin
  doTrials_found := false;
  if strategy = 'optimal' then begin
  doTrials_prev := doTrials_p;
  doTrials_d := 0;
  while doTrials_d < 50 do begin
  doTrials_this := doTrials_drawers[doTrials_prev];
  if doTrials_this = doTrials_p then begin
  doTrials_found := true;
  break;
end;
  doTrials_prev := doTrials_this;
  doTrials_d := doTrials_d + 1;
end;
end else begin
  doTrials_opened := [];
  doTrials_k := 0;
  while doTrials_k < 100 do begin
  doTrials_opened := concat(doTrials_opened, [false]);
  doTrials_k := doTrials_k + 1;
end;
  doTrials_d := 0;
  while doTrials_d < 50 do begin
  doTrials_n := _now() mod 100;
  while doTrials_opened[doTrials_n] do begin
  doTrials_n := _now() mod 100;
end;
  doTrials_opened[doTrials_n] := true;
  if doTrials_drawers[doTrials_n] = doTrials_p then begin
  doTrials_found := true;
  break;
end;
  doTrials_d := doTrials_d + 1;
end;
end;
  if not doTrials_found then begin
  doTrials_success := false;
  break;
end;
  doTrials_p := doTrials_p + 1;
end;
  if doTrials_success then begin
  doTrials_pardoned := doTrials_pardoned + 1;
end;
  doTrials_t := doTrials_t + 1;
end;
  doTrials_rf := (doTrials_pardoned div trials) * 100;
  writeln(((((('  strategy = ' + strategy) + '  pardoned = ') + IntToStr(doTrials_pardoned)) + ' relative frequency = ') + IntToStr(doTrials_rf)) + '%');
end;
procedure main();
begin
  main_trials := 1000;
  for np in [10, 100] do begin
  writeln(((('Results from ' + IntToStr(main_trials)) + ' trials with ') + IntToStr(np)) + ' prisoners:' + #10 + '');
  for strat in ['random', 'optimal'] do begin
  doTrials(main_trials, np, strat);
end;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function sortRunes(s: string): string; forward;
function deranged(a: string; b: string): boolean; forward;
procedure main(); forward;
function sortRunes(s: string): string;
var
  sortRunes_arr: array of string;
  sortRunes_i: integer;
  sortRunes_n: integer;
  sortRunes_m: integer;
  sortRunes_j: integer;
  sortRunes_tmp: string;
  sortRunes_out: string;
begin
  sortRunes_arr := [];
  sortRunes_i := 0;
  while sortRunes_i < Length(s) do begin
  sortRunes_arr := concat(sortRunes_arr, [copy(s, sortRunes_i+1, (sortRunes_i + 1 - (sortRunes_i)))]);
  sortRunes_i := sortRunes_i + 1;
end;
  sortRunes_n := Length(sortRunes_arr);
  sortRunes_m := 0;
  while sortRunes_m < sortRunes_n do begin
  sortRunes_j := 0;
  while sortRunes_j < (sortRunes_n - 1) do begin
  if sortRunes_arr[sortRunes_j] > sortRunes_arr[sortRunes_j + 1] then begin
  sortRunes_tmp := sortRunes_arr[sortRunes_j];
  sortRunes_arr[sortRunes_j] := sortRunes_arr[sortRunes_j + 1];
  sortRunes_arr[sortRunes_j + 1] := sortRunes_tmp;
end;
  sortRunes_j := sortRunes_j + 1;
end;
  sortRunes_m := sortRunes_m + 1;
end;
  sortRunes_out := '';
  sortRunes_i := 0;
  while sortRunes_i < sortRunes_n do begin
  sortRunes_out := sortRunes_out + sortRunes_arr[sortRunes_i];
  sortRunes_i := sortRunes_i + 1;
end;
  exit(sortRunes_out);
end;
function deranged(a: string; b: string): boolean;
var
  deranged_i: integer;
begin
  if Length(a) <> Length(b) then begin
  exit(false);
end;
  deranged_i := 0;
  while deranged_i < Length(a) do begin
  if copy(a, deranged_i+1, (deranged_i + 1 - (deranged_i))) = copy(b, deranged_i+1, (deranged_i + 1 - (deranged_i))) then begin
  exit(false);
end;
  deranged_i := deranged_i + 1;
end;
  exit(true);
end;
procedure main();
var
  main_words: array of string;
  main_m: specialize TFPGMap<string, StrArray>;
  main_bestLen: integer;
  main_w1: string;
  main_w2: string;
  main_w: string;
  main_k: string;
  main_c: integer;
begin
  main_words := ['constitutionalism', 'misconstitutional'];
  main_bestLen := 0;
  main_w1 := '';
  main_w2 := '';
  for main_w in main_words do begin
  if Length(main_w) <= main_bestLen then begin
  continue;
end;
  main_k := sortRunes(main_w);
  if not main_m.IndexOf(main_k) <> -1 then begin
  main_m.AddOrSetData(main_k, [main_w]);
  continue;
end;
  for main_c in main_m[main_k] do begin
  if deranged(main_w, main_c) then begin
  main_bestLen := Length(main_w);
  main_w1 := main_c;
  main_w2 := main_w;
  break;
end;
end;
  main_m.AddOrSetData(main_k, concat(main_m[main_k], [main_w]));
end;
  writeln((((main_w1 + ' ') + main_w2) + ' : Length ') + IntToStr(main_bestLen));
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

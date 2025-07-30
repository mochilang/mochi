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
function sortStrings(xs: StrArray): StrArray; forward;
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
function sortStrings(xs: StrArray): StrArray;
var
  sortStrings_res: array of string;
  sortStrings_tmp: array of string;
  sortStrings_min: string;
  sortStrings_idx: integer;
  sortStrings_i: integer;
  sortStrings_out: array of string;
  sortStrings_j: integer;
begin
  sortStrings_res := [];
  sortStrings_tmp := xs;
  while Length(sortStrings_tmp) > 0 do begin
  sortStrings_min := sortStrings_tmp[0];
  sortStrings_idx := 0;
  sortStrings_i := 1;
  while sortStrings_i < Length(sortStrings_tmp) do begin
  if sortStrings_tmp[sortStrings_i] < sortStrings_min then begin
  sortStrings_min := sortStrings_tmp[sortStrings_i];
  sortStrings_idx := sortStrings_i;
end;
  sortStrings_i := sortStrings_i + 1;
end;
  sortStrings_res := concat(sortStrings_res, [sortStrings_min]);
  sortStrings_out := [];
  sortStrings_j := 0;
  while sortStrings_j < Length(sortStrings_tmp) do begin
  if sortStrings_j <> sortStrings_idx then begin
  sortStrings_out := concat(sortStrings_out, [sortStrings_tmp[sortStrings_j]]);
end;
  sortStrings_j := sortStrings_j + 1;
end;
  sortStrings_tmp := sortStrings_out;
end;
  exit(sortStrings_res);
end;
procedure main();
var
  main_words: array of string;
  main_groups: specialize TFPGMap<string, StrArray>;
  main_maxLen: integer;
  main_w: string;
  main_k: string;
  main_printed: specialize TFPGMap<string, boolean>;
  main_g: StrArray;
  main_line: string;
  main_i: integer;
begin
  main_words := ['abel', 'able', 'bale', 'bela', 'elba', 'alger', 'glare', 'lager', 'large', 'regal', 'angel', 'angle', 'galen', 'glean', 'lange', 'caret', 'carte', 'cater', 'crate', 'trace', 'elan', 'lane', 'lean', 'lena', 'neal', 'evil', 'levi', 'live', 'veil', 'vile'];
  main_maxLen := 0;
  for main_w in main_words do begin
  main_k := sortRunes(main_w);
  if not main_groups.IndexOf(main_k) <> -1 then begin
  main_groups.AddOrSetData(main_k, [main_w]);
end else begin
  main_groups.AddOrSetData(main_k, concat(main_groups[main_k], [main_w]));
end;
  if Length(main_groups[main_k]) > main_maxLen then begin
  main_maxLen := Length(main_groups[main_k]);
end;
end;
  for main_w in main_words do begin
  main_k := sortRunes(main_w);
  if Length(main_groups[main_k]) = main_maxLen then begin
  if not main_printed.IndexOf(main_k) <> -1 then begin
  main_g := sortStrings(main_groups[main_k]);
  main_line := '[' + main_g[0];
  main_i := 1;
  while main_i < Length(main_g) do begin
  main_line := (main_line + ' ') + main_g[main_i];
  main_i := main_i + 1;
end;
  main_line := main_line + ']';
  writeln(main_line);
  main_printed.AddOrSetData(main_k, true);
end;
end;
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

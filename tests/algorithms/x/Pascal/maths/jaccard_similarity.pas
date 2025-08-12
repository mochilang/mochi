{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
  xs: StrArray;
  value: string;
  set_a: StrArray;
  alternative_union: boolean;
  set_b: StrArray;
function contains(xs: StrArray; value: string): boolean; forward;
function jaccard_similarity(set_a: StrArray; set_b: StrArray; alternative_union: boolean): real; forward;
procedure main(); forward;
function contains(xs: StrArray; value: string): boolean;
var
  contains_i: integer;
begin
  contains_i := 0;
  while contains_i < Length(xs) do begin
  if xs[contains_i] = value then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function jaccard_similarity(set_a: StrArray; set_b: StrArray; alternative_union: boolean): real;
var
  jaccard_similarity_intersection_len: integer;
  jaccard_similarity_i: integer;
  jaccard_similarity_union_len: integer;
  jaccard_similarity_union_list: array of string;
  jaccard_similarity_val_a: string;
  jaccard_similarity_val_b: string;
begin
  jaccard_similarity_intersection_len := 0;
  jaccard_similarity_i := 0;
  while jaccard_similarity_i < Length(set_a) do begin
  if contains(set_b, set_a[jaccard_similarity_i]) then begin
  jaccard_similarity_intersection_len := jaccard_similarity_intersection_len + 1;
end;
  jaccard_similarity_i := jaccard_similarity_i + 1;
end;
  jaccard_similarity_union_len := 0;
  if alternative_union then begin
  jaccard_similarity_union_len := Length(set_a) + Length(set_b);
end else begin
  jaccard_similarity_union_list := [];
  jaccard_similarity_i := 0;
  while jaccard_similarity_i < Length(set_a) do begin
  jaccard_similarity_val_a := set_a[jaccard_similarity_i];
  if not contains(jaccard_similarity_union_list, jaccard_similarity_val_a) then begin
  jaccard_similarity_union_list := concat(jaccard_similarity_union_list, StrArray([jaccard_similarity_val_a]));
end;
  jaccard_similarity_i := jaccard_similarity_i + 1;
end;
  jaccard_similarity_i := 0;
  while jaccard_similarity_i < Length(set_b) do begin
  jaccard_similarity_val_b := set_b[jaccard_similarity_i];
  if not contains(jaccard_similarity_union_list, jaccard_similarity_val_b) then begin
  jaccard_similarity_union_list := concat(jaccard_similarity_union_list, StrArray([jaccard_similarity_val_b]));
end;
  jaccard_similarity_i := jaccard_similarity_i + 1;
end;
  jaccard_similarity_union_len := Length(jaccard_similarity_union_list);
end;
  exit((1 * jaccard_similarity_intersection_len) / jaccard_similarity_union_len);
end;
procedure main();
var
  main_set_a: array of string;
  main_set_b: array of string;
begin
  main_set_a := ['a', 'b', 'c', 'd', 'e'];
  main_set_b := ['c', 'd', 'e', 'f', 'h', 'i'];
  writeln(jaccard_similarity(main_set_a, main_set_b, false));
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
end.

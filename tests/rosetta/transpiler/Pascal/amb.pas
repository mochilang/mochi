{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
type StrArrayArray = array of StrArray;
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
function amb(wordsets: StrArrayArray; res: StrArray; idx: integer): boolean; forward;
procedure main(); forward;
function amb(wordsets: StrArrayArray; res: StrArray; idx: integer): boolean;
var
  amb_prev: string;
  amb_i: integer;
  amb_w: string;
begin
  if idx = Length(wordsets) then begin
  exit(true);
end;
  amb_prev := '';
  if idx > 0 then begin
  amb_prev := res[idx - 1];
end;
  amb_i := 0;
  while amb_i < Length(wordsets[idx]) do begin
  amb_w := wordsets[idx][amb_i];
  if (idx = 0) or (copy(amb_prev, Length(amb_prev) - 1+1, (Length(amb_prev) - (Length(amb_prev) - 1))) = copy(amb_w, 0+1, (1 - (0)))) then begin
  res[idx] := amb_w;
  if amb(wordsets, res, idx + 1) then begin
  exit(true);
end;
end;
  amb_i := amb_i + 1;
end;
  exit(false);
end;
procedure main();
var
  main_wordset: array of array of string;
  main_res: array of string;
  main_i: integer;
  main_out: string;
  main_j: integer;
begin
  main_wordset := [['the', 'that', 'a'], ['frog', 'elephant', 'thing'], ['walked', 'treaded', 'grows'], ['slowly', 'quickly']];
  main_res := [];
  main_i := 0;
  while main_i < Length(main_wordset) do begin
  main_res := concat(main_res, ['']);
  main_i := main_i + 1;
end;
  if amb(main_wordset, main_res, 0) then begin
  main_out := '[' + main_res[0];
  main_j := 1;
  while main_j < Length(main_res) do begin
  main_out := (main_out + ' ') + main_res[main_j];
  main_j := main_j + 1;
end;
  main_out := main_out + ']';
  writeln(main_out);
end else begin
  writeln('No amb found');
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

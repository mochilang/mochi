{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  a: array of integer;
  b: array of integer;
  i: array of Variant;
  j: array of Variant;
  l: array of integer;
  m: array of integer;
function concatInts(a: IntArray; b: IntArray): IntArray; forward;
function concatAny(a: VariantArray; b: VariantArray): VariantArray; forward;
function concatInts(a: IntArray; b: IntArray): IntArray;
var
  concatInts_out: array of integer;
  concatInts_v: integer;
begin
  concatInts_out := [];
  for concatInts_v in a do begin
  concatInts_out := concat(concatInts_out, [concatInts_v]);
end;
  for concatInts_v in b do begin
  concatInts_out := concat(concatInts_out, [concatInts_v]);
end;
  exit(concatInts_out);
end;
function concatAny(a: VariantArray; b: VariantArray): VariantArray;
var
  concatAny_out: array of Variant;
  concatAny_v: integer;
begin
  concatAny_out := [];
  for concatAny_v in a do begin
  concatAny_out := concat(concatAny_out, [concatAny_v]);
end;
  for concatAny_v in b do begin
  concatAny_out := concat(concatAny_out, [concatAny_v]);
end;
  exit(concatAny_out);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  a := [1, 2, 3];
  b := [7, 12, 60];
  writeln(list_int_to_str(concatInts(a, b)));
  i := [1, 2, 3];
  j := ['Crosby', 'Stills', 'Nash', 'Young'];
  writeln(list_int_to_str(concatAny(i, j)));
  l := [1, 2, 3];
  m := [7, 12, 60];
  writeln(list_int_to_str(concatInts(l, m)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

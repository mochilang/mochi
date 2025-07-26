{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  plus_res: array of integer;
  plus_i: integer;
  v: integer;
  topple_neighbors: IntArrayArray;
  topple_i: integer;
  topple_nbs: IntArray;
  j: integer;
  pileString_s: string;
  pileString_r: integer;
  pileString_c: integer;
  s4: array of integer;
  s1: array of integer;
  s2: array of integer;
  s3_a: IntArray;
  s3_b: IntArray;
  s3: array of integer;
  s3_id: array of integer;
  s4b: IntArray;
  s5: IntArray;
function neighborsList(): IntArrayArray; forward;
function plus(a: IntArray; b: IntArray): IntArray; forward;
function isStable(p: IntArray): boolean; forward;
function topple(p: IntArray): integer; forward;
function pileString(p: IntArray): string; forward;
function neighborsList(): IntArrayArray;
begin
  exit([[1, 3], [0, 2, 4], [1, 5], [0, 4, 6], [1, 3, 5, 7], [2, 4, 8], [3, 7], [4, 6, 8], [5, 7]]);
end;
function plus(a: IntArray; b: IntArray): IntArray;
begin
  plus_res := [];
  plus_i := 0;
  while plus_i < Length(a) do begin
  plus_res := concat(plus_res, [a[plus_i] + b[plus_i]]);
  plus_i := plus_i + 1;
end;
  exit(plus_res);
end;
function isStable(p: IntArray): boolean;
begin
  for v in p do begin
  if v > 3 then begin
  exit(false);
end;
end;
  exit(true);
end;
function topple(p: IntArray): integer;
begin
  topple_neighbors := neighborsList();
  topple_i := 0;
  while topple_i < Length(p) do begin
  if p[topple_i] > 3 then begin
  p[topple_i] := p[topple_i] - 4;
  topple_nbs := topple_neighbors[topple_i];
  for j in topple_nbs do begin
  p[j] := p[j] + 1;
end;
  exit(0);
end;
  topple_i := topple_i + 1;
end;
  exit(0);
end;
function pileString(p: IntArray): string;
begin
  pileString_s := '';
  pileString_r := 0;
  while pileString_r < 3 do begin
  pileString_c := 0;
  while pileString_c < 3 do begin
  pileString_s := (pileString_s + IntToStr(p[(3 * pileString_r) + pileString_c])) + ' ';
  pileString_c := pileString_c + 1;
end;
  pileString_s := pileString_s + '' + #10 + '';
  pileString_r := pileString_r + 1;
end;
  exit(pileString_s);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln('Avalanche of topplings:' + #10 + '');
  s4 := [4, 3, 3, 3, 1, 2, 0, 2, 3];
  writeln(pileString(s4));
  while not isStable(s4) do begin
  topple(s4);
  writeln(pileString(s4));
end;
  writeln('Commutative additions:' + #10 + '');
  s1 := [1, 2, 0, 2, 1, 1, 0, 1, 3];
  s2 := [2, 1, 3, 1, 0, 1, 0, 1, 0];
  s3_a := plus(s1, s2);
  while not isStable(s3_a) do begin
  topple(s3_a);
end;
  s3_b := plus(s2, s1);
  while not isStable(s3_b) do begin
  topple(s3_b);
end;
  writeln((((pileString(s1) + '' + #10 + 'plus' + #10 + '' + #10 + '') + pileString(s2)) + '' + #10 + 'equals' + #10 + '' + #10 + '') + pileString(s3_a));
  writeln((((('and' + #10 + '' + #10 + '' + pileString(s2)) + '' + #10 + 'plus' + #10 + '' + #10 + '') + pileString(s1)) + '' + #10 + 'also equals' + #10 + '' + #10 + '') + pileString(s3_b));
  writeln('Addition of identity sandpile:' + #10 + '');
  s3 := [3, 3, 3, 3, 3, 3, 3, 3, 3];
  s3_id := [2, 1, 2, 1, 0, 1, 2, 1, 2];
  s4b := plus(s3, s3_id);
  while not isStable(s4b) do begin
  topple(s4b);
end;
  writeln((((pileString(s3) + '' + #10 + 'plus' + #10 + '' + #10 + '') + pileString(s3_id)) + '' + #10 + 'equals' + #10 + '' + #10 + '') + pileString(s4b));
  writeln('Addition of identities:' + #10 + '');
  s5 := plus(s3_id, s3_id);
  while not isStable(s5) do begin
  topple(s5);
end;
  writeln((((pileString(s3_id) + '' + #10 + 'plus' + #10 + '' + #10 + '') + pileString(s3_id)) + '' + #10 + 'equals' + #10 + '' + #10 + '') + pileString(s5));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

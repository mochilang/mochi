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
  powInt_r: integer;
  powInt_b: integer;
  powInt_e: integer;
  throwDie_i: integer;
  beatingProbability_len1: integer;
  beatingProbability_c1: array of integer;
  beatingProbability_i: integer;
  beatingProbability_len2: integer;
  beatingProbability_c2: array of integer;
  beatingProbability_j: integer;
  beatingProbability_p12: real;
  beatingProbability_tot: real;
  beatingProbability_m: integer;
function powInt(base: integer; exp: integer): integer; forward;
function minInt(x: integer; y: integer): integer; forward;
procedure throwDie(nSides: integer; nDice: integer; s: integer; var counts: IntArray); forward;
function beatingProbability(nSides1: integer; nDice1: integer; nSides2: integer; nDice2: integer): real; forward;
function powInt(base: integer; exp: integer): integer;
begin
  powInt_r := 1;
  powInt_b := base;
  powInt_e := exp;
  while powInt_e > 0 do begin
  if (powInt_e mod 2) = 1 then begin
  powInt_r := powInt_r * powInt_b;
end;
  powInt_b := powInt_b * powInt_b;
  powInt_e := powInt_e div Trunc(2);
end;
  exit(powInt_r);
end;
function minInt(x: integer; y: integer): integer;
begin
  if x < y then begin
  exit(x);
end;
  exit(y);
end;
procedure throwDie(nSides: integer; nDice: integer; s: integer; var counts: IntArray);
begin
  if nDice = 0 then begin
  counts[s] := counts[s] + 1;
  exit();
end;
  throwDie_i := 1;
  while throwDie_i <= nSides do begin
  throwDie(nSides, nDice - 1, s + throwDie_i, counts);
  throwDie_i := throwDie_i + 1;
end;
end;
function beatingProbability(nSides1: integer; nDice1: integer; nSides2: integer; nDice2: integer): real;
begin
  beatingProbability_len1 := (nSides1 + 1) * nDice1;
  beatingProbability_c1 := [];
  beatingProbability_i := 0;
  while beatingProbability_i < beatingProbability_len1 do begin
  beatingProbability_c1 := concat(beatingProbability_c1, [0]);
  beatingProbability_i := beatingProbability_i + 1;
end;
  throwDie(nSides1, nDice1, 0, beatingProbability_c1);
  beatingProbability_len2 := (nSides2 + 1) * nDice2;
  beatingProbability_c2 := [];
  beatingProbability_j := 0;
  while beatingProbability_j < beatingProbability_len2 do begin
  beatingProbability_c2 := concat(beatingProbability_c2, [0]);
  beatingProbability_j := beatingProbability_j + 1;
end;
  throwDie(nSides2, nDice2, 0, beatingProbability_c2);
  beatingProbability_p12 := Double(powInt(nSides1, nDice1)) * Double(powInt(nSides2, nDice2));
  beatingProbability_tot := 0;
  beatingProbability_i := 0;
  while beatingProbability_i < beatingProbability_len1 do begin
  beatingProbability_j := 0;
  beatingProbability_m := minInt(beatingProbability_i, beatingProbability_len2);
  while beatingProbability_j < beatingProbability_m do begin
  beatingProbability_tot := beatingProbability_tot + ((beatingProbability_c1[beatingProbability_i] * Double(beatingProbability_c2[beatingProbability_j])) / beatingProbability_p12);
  beatingProbability_j := beatingProbability_j + 1;
end;
  beatingProbability_i := beatingProbability_i + 1;
end;
  exit(beatingProbability_tot);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(beatingProbability(4, 9, 6, 6)));
  writeln(FloatToStr(beatingProbability(10, 5, 7, 6)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

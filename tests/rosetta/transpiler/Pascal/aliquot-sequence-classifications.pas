{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
  THRESHOLD: integer;
function Map1(): specialize TFPGMap<string, Variant>; forward;
function indexOf(xs: IntArray; value: integer): integer; forward;
function contains(xs: IntArray; value: integer): boolean; forward;
function maxOf(a: integer; b: integer): integer; forward;
function intSqrt(n: integer): integer; forward;
function sumProperDivisors(n: integer): integer; forward;
function classifySequence(k: integer): specialize TFPGMap<string, Variant>; forward;
function padLeft(n: integer; w: integer): string; forward;
function padRight(s: string; w: integer): string; forward;
function joinWithCommas(seq: IntArray): string; forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('seq', classifySequence_seq);
  Result.AddOrSetData('aliquot', classifySequence_aliquot);
end;
function indexOf(xs: IntArray; value: integer): integer;
var
  indexOf_i: integer;
begin
  indexOf_i := 0;
  while indexOf_i < Length(xs) do begin
  if xs[indexOf_i] = value then begin
  exit(indexOf_i);
end;
  indexOf_i := indexOf_i + 1;
end;
  exit(0 - 1);
end;
function contains(xs: IntArray; value: integer): boolean;
begin
  exit(indexOf(xs, value) <> (0 - 1));
end;
function maxOf(a: integer; b: integer): integer;
begin
  if a > b then begin
  exit(a);
end else begin
  exit(b);
end;
end;
function intSqrt(n: integer): integer;
var
  intSqrt_x: integer;
  intSqrt_y: integer;
begin
  if n = 0 then begin
  exit(0);
end;
  intSqrt_x := n;
  intSqrt_y := (intSqrt_x + 1) div 2;
  while intSqrt_y < intSqrt_x do begin
  intSqrt_x := intSqrt_y;
  intSqrt_y := (intSqrt_x + (n div intSqrt_x)) div 2;
end;
  exit(intSqrt_x);
end;
function sumProperDivisors(n: integer): integer;
var
  sumProperDivisors_sqrt: integer;
  sumProperDivisors_sum: integer;
  sumProperDivisors_i: integer;
begin
  if n < 2 then begin
  exit(0);
end;
  sumProperDivisors_sqrt := intSqrt(n);
  sumProperDivisors_sum := 1;
  sumProperDivisors_i := 2;
  while sumProperDivisors_i <= sumProperDivisors_sqrt do begin
  if (n mod sumProperDivisors_i) = 0 then begin
  sumProperDivisors_sum := (sumProperDivisors_sum + sumProperDivisors_i) + (n div sumProperDivisors_i);
end;
  sumProperDivisors_i := sumProperDivisors_i + 1;
end;
  if (sumProperDivisors_sqrt * sumProperDivisors_sqrt) = n then begin
  sumProperDivisors_sum := sumProperDivisors_sum - sumProperDivisors_sqrt;
end;
  exit(sumProperDivisors_sum);
end;
function classifySequence(k: integer): specialize TFPGMap<string, Variant>;
var
  classifySequence_last: integer;
  classifySequence_seq: array of integer;
  classifySequence_n: integer;
  classifySequence_aliquot: string;
  classifySequence_idx: integer;
begin
  classifySequence_last := k;
  classifySequence_seq := [k];
  while true do begin
  classifySequence_last := sumProperDivisors(classifySequence_last);
  classifySequence_seq := concat(classifySequence_seq, [classifySequence_last]);
  classifySequence_n := Length(classifySequence_seq);
  classifySequence_aliquot := '';
  if classifySequence_last = 0 then begin
  classifySequence_aliquot := 'Terminating';
end else begin
  if (classifySequence_n = 2) and (classifySequence_last = k) then begin
  classifySequence_aliquot := 'Perfect';
end else begin
  if (classifySequence_n = 3) and (classifySequence_last = k) then begin
  classifySequence_aliquot := 'Amicable';
end else begin
  if (classifySequence_n >= 4) and (classifySequence_last = k) then begin
  classifySequence_aliquot := ('Sociable[' + IntToStr(classifySequence_n - 1)) + ']';
end else begin
  if classifySequence_last = classifySequence_seq[classifySequence_n - 2] then begin
  classifySequence_aliquot := 'Aspiring';
end else begin
  if contains(copy(classifySequence_seq, 1, (maxOf(1, classifySequence_n - 2) - (1))), classifySequence_last) then begin
  classifySequence_idx := indexOf(classifySequence_seq, classifySequence_last);
  classifySequence_aliquot := ('Cyclic[' + IntToStr((classifySequence_n - 1) - classifySequence_idx)) + ']';
end else begin
  if (classifySequence_n = 16) or (classifySequence_last > THRESHOLD) then begin
  classifySequence_aliquot := 'Non-Terminating';
end;
end;
end;
end;
end;
end;
end;
  if classifySequence_aliquot <> '' then begin
  exit(Map1());
end;
end;
  exit(Map1());
end;
function padLeft(n: integer; w: integer): string;
var
  padLeft_s: string;
begin
  padLeft_s := IntToStr(n);
  while Length(padLeft_s) < w do begin
  padLeft_s := ' ' + padLeft_s;
end;
  exit(padLeft_s);
end;
function padRight(s: string; w: integer): string;
var
  padRight_r: string;
begin
  padRight_r := s;
  while Length(padRight_r) < w do begin
  padRight_r := padRight_r + ' ';
end;
  exit(padRight_r);
end;
function joinWithCommas(seq: IntArray): string;
var
  joinWithCommas_s: string;
  joinWithCommas_i: integer;
begin
  joinWithCommas_s := '[';
  joinWithCommas_i := 0;
  while joinWithCommas_i < Length(seq) do begin
  joinWithCommas_s := joinWithCommas_s + IntToStr(seq[joinWithCommas_i]);
  if joinWithCommas_i < (Length(seq) - 1) then begin
  joinWithCommas_s := joinWithCommas_s + ', ';
end;
  joinWithCommas_i := joinWithCommas_i + 1;
end;
  joinWithCommas_s := joinWithCommas_s + ']';
  exit(joinWithCommas_s);
end;
procedure main();
var
  main_k: integer;
  main_res: specialize TFPGMap<string, Variant>;
  main_s: array of integer;
  main_i: integer;
  main_val: integer;
  main_big: integer;
  main_r: specialize TFPGMap<string, Variant>;
begin
  writeln('Aliquot classifications - periods for Sociable/Cyclic in square brackets:' + #10 + '');
  main_k := 1;
  while main_k <= 10 do begin
  main_res := classifySequence(main_k);
  writeln((((padLeft(main_k, 2) + ': ') + padRight(main_res['aliquot'], 15)) + ' ') + joinWithCommas(main_res['seq']));
  main_k := main_k + 1;
end;
  writeln('');
  main_s := [11, 12, 28, 496, 220, 1184, 12496, 1264460, 790, 909, 562, 1064, 1488];
  main_i := 0;
  while main_i < Length(main_s) do begin
  main_val := main_s[main_i];
  main_res := classifySequence(main_val);
  writeln((((padLeft(main_val, 7) + ': ') + padRight(main_res['aliquot'], 15)) + ' ') + joinWithCommas(main_res['seq']));
  main_i := main_i + 1;
end;
  writeln('');
  main_big := 15355717786080;
  main_r := classifySequence(main_big);
  writeln((((IntToStr(main_big) + ': ') + padRight(main_r['aliquot'], 15)) + ' ') + joinWithCommas(main_r['seq']));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  THRESHOLD := 140737488355328;
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

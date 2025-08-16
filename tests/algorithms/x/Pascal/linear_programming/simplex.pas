{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, fgl;
type RealArray = array of real;
type IntArray = array of integer;
type RealArrayArray = array of RealArray;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  tableau: array of RealArray;
  finalTab: RealArrayArray;
  res: specialize TFPGMap<string, real>;
  i: int64;
  key: string;
  row: integer;
  col: integer;
  t: RealArrayArray;
  tab: RealArrayArray;
  nVars: integer;
function pivot(t: RealArrayArray; row: integer; col: integer): RealArrayArray; forward;
function findPivot(t: RealArrayArray): IntArray; forward;
function interpret(t: RealArrayArray; nVars: integer): specialize TFPGMap<string, real>; forward;
function simplex(tab: RealArrayArray): RealArrayArray; forward;
function pivot(t: RealArrayArray; row: integer; col: integer): RealArrayArray;
var
  pivot_pivotRow: array of real;
  pivot_pivotVal: real;
  pivot_j: int64;
  pivot_i: int64;
  pivot_factor: real;
  pivot_newRow: array of real;
  pivot_value: real;
begin
  pivot_pivotRow := [];
  pivot_pivotVal := t[row][col];
  for pivot_j := 0 to (Length(t[row]) - 1) do begin
  pivot_pivotRow := concat(pivot_pivotRow, [t[row][pivot_j] / pivot_pivotVal]);
end;
  t[row] := pivot_pivotRow;
  for pivot_i := 0 to (Length(t) - 1) do begin
  if pivot_i <> row then begin
  pivot_factor := t[pivot_i][col];
  pivot_newRow := [];
  for pivot_j := 0 to (Length(t[pivot_i]) - 1) do begin
  pivot_value := t[pivot_i][pivot_j] - (pivot_factor * pivot_pivotRow[pivot_j]);
  pivot_newRow := concat(pivot_newRow, [pivot_value]);
end;
  t[pivot_i] := pivot_newRow;
end;
end;
  exit(t);
end;
function findPivot(t: RealArrayArray): IntArray;
var
  findPivot_col: integer;
  findPivot_minVal: real;
  findPivot_j: int64;
  findPivot_v: real;
  findPivot_row: integer;
  findPivot_minRatio: real;
  findPivot_first: boolean;
  findPivot_i: int64;
  findPivot_coeff: real;
  findPivot_rhs: real;
  findPivot_ratio: real;
begin
  findPivot_col := 0;
  findPivot_minVal := 0;
  for findPivot_j := 0 to (Length(t[0]) - 1 - 1) do begin
  findPivot_v := t[0][findPivot_j];
  if findPivot_v < findPivot_minVal then begin
  findPivot_minVal := findPivot_v;
  findPivot_col := findPivot_j;
end;
end;
  if findPivot_minVal >= 0 then begin
  exit([-1, -1]);
end;
  findPivot_row := -1;
  findPivot_minRatio := 0;
  findPivot_first := true;
  for findPivot_i := 1 to (Length(t) - 1) do begin
  findPivot_coeff := t[findPivot_i][findPivot_col];
  if findPivot_coeff > 0 then begin
  findPivot_rhs := t[findPivot_i][Length(t[findPivot_i]) - 1];
  findPivot_ratio := findPivot_rhs / findPivot_coeff;
  if findPivot_first or (findPivot_ratio < findPivot_minRatio) then begin
  findPivot_minRatio := findPivot_ratio;
  findPivot_row := findPivot_i;
  findPivot_first := false;
end;
end;
end;
  exit([findPivot_row, findPivot_col]);
end;
function interpret(t: RealArrayArray; nVars: integer): specialize TFPGMap<string, real>;
var
  interpret_lastCol: integer;
  interpret_p: real;
  interpret_result_: specialize TFPGMap<string, real>;
  interpret_i: int64;
  interpret_nzRow: integer;
  interpret_nzCount: integer;
  interpret_r: int64;
  interpret_val: real;
begin
  interpret_lastCol := Length(t[0]) - 1;
  interpret_p := t[0][interpret_lastCol];
  if interpret_p < 0 then begin
  interpret_p := -interpret_p;
end;
  interpret_result_ := specialize TFPGMap<string, real>.Create();
  interpret_result_['P'] := interpret_p;
  for interpret_i := 0 to (nVars - 1) do begin
  interpret_nzRow := -1;
  interpret_nzCount := 0;
  for interpret_r := 0 to (Length(t) - 1) do begin
  interpret_val := t[interpret_r][interpret_i];
  if interpret_val <> 0 then begin
  interpret_nzCount := interpret_nzCount + 1;
  interpret_nzRow := interpret_r;
end;
end;
  if (interpret_nzCount = 1) and (t[interpret_nzRow][interpret_i] = 1) then begin
  interpret_result_['x' + IntToStr(interpret_i + 1)] := t[interpret_nzRow][interpret_lastCol];
end;
end;
  exit(interpret_result_);
end;
function simplex(tab: RealArrayArray): RealArrayArray;
var
  simplex_t: array of RealArray;
  simplex_p: IntArray;
  simplex_row: integer;
  simplex_col: integer;
begin
  simplex_t := tab;
  while true do begin
  simplex_p := findPivot(simplex_t);
  simplex_row := simplex_p[0];
  simplex_col := simplex_p[1];
  if simplex_row < 0 then begin
  break;
end;
  simplex_t := pivot(simplex_t, simplex_row, simplex_col);
end;
  exit(simplex_t);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  tableau := [[-1, -1, 0, 0, 0], [1, 3, 1, 0, 4], [3, 1, 0, 1, 4]];
  finalTab := simplex(tableau);
  res := interpret(finalTab, 2);
  writeln('P: ' + FloatToStr(res['P']));
  for i := 0 to (2 - 1) do begin
  key := 'x' + IntToStr(i + 1);
  if res.IndexOf(key) <> -1 then begin
  writeln((key + ': ') + FloatToStr(res[key]));
end;
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type RealArray = array of real;
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
procedure show_list_real(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
function list_real_to_str(xs: array of real): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + FloatToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function cramers_rule_2x2(cramers_rule_2x2_eq1: RealArray; cramers_rule_2x2_eq2: RealArray): RealArray; forward;
procedure test_cramers_rule_2x2(); forward;
procedure main(); forward;
function cramers_rule_2x2(cramers_rule_2x2_eq1: RealArray; cramers_rule_2x2_eq2: RealArray): RealArray;
var
  cramers_rule_2x2_a1: real;
  cramers_rule_2x2_b1: real;
  cramers_rule_2x2_c1: real;
  cramers_rule_2x2_a2: real;
  cramers_rule_2x2_b2: real;
  cramers_rule_2x2_c2: real;
  cramers_rule_2x2_determinant: real;
  cramers_rule_2x2_determinant_x: real;
  cramers_rule_2x2_determinant_y: real;
  cramers_rule_2x2_x: real;
  cramers_rule_2x2_y: real;
begin
  if (Length(cramers_rule_2x2_eq1) <> 3) or (Length(cramers_rule_2x2_eq2) <> 3) then begin
  panic('Please enter a valid equation.');
end;
  if (((cramers_rule_2x2_eq1[0] = 0) and (cramers_rule_2x2_eq1[1] = 0)) and (cramers_rule_2x2_eq2[0] = 0)) and (cramers_rule_2x2_eq2[1] = 0) then begin
  panic('Both a & b of two equations can''t be zero.');
end;
  cramers_rule_2x2_a1 := cramers_rule_2x2_eq1[0];
  cramers_rule_2x2_b1 := cramers_rule_2x2_eq1[1];
  cramers_rule_2x2_c1 := cramers_rule_2x2_eq1[2];
  cramers_rule_2x2_a2 := cramers_rule_2x2_eq2[0];
  cramers_rule_2x2_b2 := cramers_rule_2x2_eq2[1];
  cramers_rule_2x2_c2 := cramers_rule_2x2_eq2[2];
  cramers_rule_2x2_determinant := (cramers_rule_2x2_a1 * cramers_rule_2x2_b2) - (cramers_rule_2x2_a2 * cramers_rule_2x2_b1);
  cramers_rule_2x2_determinant_x := (cramers_rule_2x2_c1 * cramers_rule_2x2_b2) - (cramers_rule_2x2_c2 * cramers_rule_2x2_b1);
  cramers_rule_2x2_determinant_y := (cramers_rule_2x2_a1 * cramers_rule_2x2_c2) - (cramers_rule_2x2_a2 * cramers_rule_2x2_c1);
  if cramers_rule_2x2_determinant = 0 then begin
  if (cramers_rule_2x2_determinant_x = 0) and (cramers_rule_2x2_determinant_y = 0) then begin
  panic('Infinite solutions. (Consistent system)');
end;
  panic('No solution. (Inconsistent system)');
end;
  if (cramers_rule_2x2_determinant_x = 0) and (cramers_rule_2x2_determinant_y = 0) then begin
  exit([0, 0]);
end;
  cramers_rule_2x2_x := cramers_rule_2x2_determinant_x / cramers_rule_2x2_determinant;
  cramers_rule_2x2_y := cramers_rule_2x2_determinant_y / cramers_rule_2x2_determinant;
  exit([cramers_rule_2x2_x, cramers_rule_2x2_y]);
end;
procedure test_cramers_rule_2x2();
var
  test_cramers_rule_2x2_r1: RealArray;
  test_cramers_rule_2x2_r2: RealArray;
begin
  test_cramers_rule_2x2_r1 := cramers_rule_2x2([2, 3, 0], [5, 1, 0]);
  if (test_cramers_rule_2x2_r1[0] <> 0) or (test_cramers_rule_2x2_r1[1] <> 0) then begin
  panic('Test1 failed');
end;
  test_cramers_rule_2x2_r2 := cramers_rule_2x2([0, 4, 50], [2, 0, 26]);
  if (test_cramers_rule_2x2_r2[0] <> 13) or (test_cramers_rule_2x2_r2[1] <> 12.5) then begin
  panic('Test2 failed');
end;
end;
procedure main();
begin
  test_cramers_rule_2x2();
  show_list_real(cramers_rule_2x2([11, 2, 30], [1, 0, 4]));
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

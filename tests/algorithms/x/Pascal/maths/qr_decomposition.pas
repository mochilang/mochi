{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
type RealArray = array of real;
type RealArrayArray = array of RealArray;
type QR = record
  q: array of RealArray;
  r: array of RealArray;
end;
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
  A_48: array of RealArray;
  result_: QR;
  a: RealArrayArray;
  b: RealArrayArray;
  mat: RealArrayArray;
  n: integer;
  v: RealArray;
  x: real;
function makeQR(q: RealArrayArray; r: RealArrayArray): QR; forward;
function sqrt_approx(x: real): real; forward;
function sign(x: real): real; forward;
function vector_norm(v: RealArray): real; forward;
function identity_matrix(n: integer): RealArrayArray; forward;
function copy_matrix(a: RealArrayArray): RealArrayArray; forward;
function matmul(a: RealArrayArray; b: RealArrayArray): RealArrayArray; forward;
function qr_decomposition(a: RealArrayArray): QR; forward;
procedure print_matrix(mat: RealArrayArray); forward;
function makeQR(q: RealArrayArray; r: RealArrayArray): QR;
begin
  Result.q := q;
  Result.r := r;
end;
function sqrt_approx(x: real): real;
var
  sqrt_approx_guess: real;
  sqrt_approx_i: integer;
begin
  if x <= 0 then begin
  exit(0);
end;
  sqrt_approx_guess := x;
  sqrt_approx_i := 0;
  while sqrt_approx_i < 20 do begin
  sqrt_approx_guess := (sqrt_approx_guess + (x / sqrt_approx_guess)) / 2;
  sqrt_approx_i := sqrt_approx_i + 1;
end;
  exit(sqrt_approx_guess);
end;
function sign(x: real): real;
begin
  if x >= 0 then begin
  exit(1);
end else begin
  exit(-1);
end;
end;
function vector_norm(v: RealArray): real;
var
  vector_norm_sum: real;
  vector_norm_i: integer;
  vector_norm_n: real;
begin
  vector_norm_sum := 0;
  vector_norm_i := 0;
  while vector_norm_i < Length(v) do begin
  vector_norm_sum := vector_norm_sum + (v[vector_norm_i] * v[vector_norm_i]);
  vector_norm_i := vector_norm_i + 1;
end;
  vector_norm_n := sqrt_approx(vector_norm_sum);
  exit(vector_norm_n);
end;
function identity_matrix(n: integer): RealArrayArray;
var
  identity_matrix_mat: array of RealArray;
  identity_matrix_i: integer;
  identity_matrix_row: array of real;
  identity_matrix_j: integer;
begin
  identity_matrix_mat := [];
  identity_matrix_i := 0;
  while identity_matrix_i < n do begin
  identity_matrix_row := [];
  identity_matrix_j := 0;
  while identity_matrix_j < n do begin
  if identity_matrix_i = identity_matrix_j then begin
  identity_matrix_row := concat(identity_matrix_row, [1]);
end else begin
  identity_matrix_row := concat(identity_matrix_row, [0]);
end;
  identity_matrix_j := identity_matrix_j + 1;
end;
  identity_matrix_mat := concat(identity_matrix_mat, [identity_matrix_row]);
  identity_matrix_i := identity_matrix_i + 1;
end;
  exit(identity_matrix_mat);
end;
function copy_matrix(a: RealArrayArray): RealArrayArray;
var
  copy_matrix_mat: array of RealArray;
  copy_matrix_i: integer;
  copy_matrix_row: array of real;
  copy_matrix_j: integer;
begin
  copy_matrix_mat := [];
  copy_matrix_i := 0;
  while copy_matrix_i < Length(a) do begin
  copy_matrix_row := [];
  copy_matrix_j := 0;
  while copy_matrix_j < Length(a[copy_matrix_i]) do begin
  copy_matrix_row := concat(copy_matrix_row, [a[copy_matrix_i][copy_matrix_j]]);
  copy_matrix_j := copy_matrix_j + 1;
end;
  copy_matrix_mat := concat(copy_matrix_mat, [copy_matrix_row]);
  copy_matrix_i := copy_matrix_i + 1;
end;
  exit(copy_matrix_mat);
end;
function matmul(a: RealArrayArray; b: RealArrayArray): RealArrayArray;
var
  matmul_m: integer;
  matmul_n: integer;
  matmul_p: integer;
  matmul_res: array of RealArray;
  matmul_i: integer;
  matmul_row: array of real;
  matmul_j: integer;
  matmul_sum: real;
  matmul_k: integer;
begin
  matmul_m := Length(a);
  matmul_n := Length(a[0]);
  matmul_p := Length(b[0]);
  matmul_res := [];
  matmul_i := 0;
  while matmul_i < matmul_m do begin
  matmul_row := [];
  matmul_j := 0;
  while matmul_j < matmul_p do begin
  matmul_sum := 0;
  matmul_k := 0;
  while matmul_k < matmul_n do begin
  matmul_sum := matmul_sum + (a[matmul_i][matmul_k] * b[matmul_k][matmul_j]);
  matmul_k := matmul_k + 1;
end;
  matmul_row := concat(matmul_row, [matmul_sum]);
  matmul_j := matmul_j + 1;
end;
  matmul_res := concat(matmul_res, [matmul_row]);
  matmul_i := matmul_i + 1;
end;
  exit(matmul_res);
end;
function qr_decomposition(a: RealArrayArray): QR;
var
  qr_decomposition_m: integer;
  qr_decomposition_n: integer;
  qr_decomposition_t: integer;
  qr_decomposition_q: RealArrayArray;
  qr_decomposition_r: RealArrayArray;
  qr_decomposition_k: integer;
  qr_decomposition_x: array of real;
  qr_decomposition_i: integer;
  qr_decomposition_e1: array of real;
  qr_decomposition_alpha: real;
  qr_decomposition_s: real;
  qr_decomposition_v: array of real;
  qr_decomposition_vnorm: real;
  qr_decomposition_size: integer;
  qr_decomposition_qk_small: array of RealArray;
  qr_decomposition_row: array of real;
  qr_decomposition_j: integer;
  qr_decomposition_delta: real;
  qr_decomposition_qk: RealArrayArray;
begin
  qr_decomposition_m := Length(a);
  qr_decomposition_n := Length(a[0]);
  if qr_decomposition_m < qr_decomposition_n then begin
  qr_decomposition_t := qr_decomposition_m;
end else begin
  qr_decomposition_t := qr_decomposition_n;
end;
  qr_decomposition_q := identity_matrix(qr_decomposition_m);
  qr_decomposition_r := copy_matrix(a);
  qr_decomposition_k := 0;
  while qr_decomposition_k < (qr_decomposition_t - 1) do begin
  qr_decomposition_x := [];
  qr_decomposition_i := qr_decomposition_k;
  while qr_decomposition_i < qr_decomposition_m do begin
  qr_decomposition_x := concat(qr_decomposition_x, [qr_decomposition_r[qr_decomposition_i][qr_decomposition_k]]);
  qr_decomposition_i := qr_decomposition_i + 1;
end;
  qr_decomposition_e1 := [];
  qr_decomposition_i := 0;
  while qr_decomposition_i < Length(qr_decomposition_x) do begin
  if qr_decomposition_i = 0 then begin
  qr_decomposition_e1 := concat(qr_decomposition_e1, [1]);
end else begin
  qr_decomposition_e1 := concat(qr_decomposition_e1, [0]);
end;
  qr_decomposition_i := qr_decomposition_i + 1;
end;
  qr_decomposition_alpha := vector_norm(qr_decomposition_x);
  qr_decomposition_s := sign(qr_decomposition_x[0]) * qr_decomposition_alpha;
  qr_decomposition_v := [];
  qr_decomposition_i := 0;
  while qr_decomposition_i < Length(qr_decomposition_x) do begin
  qr_decomposition_v := concat(qr_decomposition_v, [qr_decomposition_x[qr_decomposition_i] + (qr_decomposition_s * qr_decomposition_e1[qr_decomposition_i])]);
  qr_decomposition_i := qr_decomposition_i + 1;
end;
  qr_decomposition_vnorm := vector_norm(qr_decomposition_v);
  qr_decomposition_i := 0;
  while qr_decomposition_i < Length(qr_decomposition_v) do begin
  qr_decomposition_v[qr_decomposition_i] := qr_decomposition_v[qr_decomposition_i] / qr_decomposition_vnorm;
  qr_decomposition_i := qr_decomposition_i + 1;
end;
  qr_decomposition_size := Length(qr_decomposition_v);
  qr_decomposition_qk_small := [];
  qr_decomposition_i := 0;
  while qr_decomposition_i < qr_decomposition_size do begin
  qr_decomposition_row := [];
  qr_decomposition_j := 0;
  while qr_decomposition_j < qr_decomposition_size do begin
  if qr_decomposition_i = qr_decomposition_j then begin
  qr_decomposition_delta := 1;
end else begin
  qr_decomposition_delta := 0;
end;
  qr_decomposition_row := concat(qr_decomposition_row, [qr_decomposition_delta - ((2 * qr_decomposition_v[qr_decomposition_i]) * qr_decomposition_v[qr_decomposition_j])]);
  qr_decomposition_j := qr_decomposition_j + 1;
end;
  qr_decomposition_qk_small := concat(qr_decomposition_qk_small, [qr_decomposition_row]);
  qr_decomposition_i := qr_decomposition_i + 1;
end;
  qr_decomposition_qk := identity_matrix(qr_decomposition_m);
  qr_decomposition_i := 0;
  while qr_decomposition_i < qr_decomposition_size do begin
  qr_decomposition_j := 0;
  while qr_decomposition_j < qr_decomposition_size do begin
  qr_decomposition_qk[qr_decomposition_k + qr_decomposition_i][qr_decomposition_k + qr_decomposition_j] := qr_decomposition_qk_small[qr_decomposition_i][qr_decomposition_j];
  qr_decomposition_j := qr_decomposition_j + 1;
end;
  qr_decomposition_i := qr_decomposition_i + 1;
end;
  qr_decomposition_q := matmul(qr_decomposition_q, qr_decomposition_qk);
  qr_decomposition_r := matmul(qr_decomposition_qk, qr_decomposition_r);
  qr_decomposition_k := qr_decomposition_k + 1;
end;
  exit(makeQR(qr_decomposition_q, qr_decomposition_r));
end;
procedure print_matrix(mat: RealArrayArray);
var
  print_matrix_i: integer;
  print_matrix_line: string;
  print_matrix_j: integer;
begin
  print_matrix_i := 0;
  while print_matrix_i < Length(mat) do begin
  print_matrix_line := '';
  print_matrix_j := 0;
  while print_matrix_j < Length(mat[print_matrix_i]) do begin
  print_matrix_line := print_matrix_line + FloatToStr(mat[print_matrix_i][print_matrix_j]);
  if (print_matrix_j + 1) < Length(mat[print_matrix_i]) then begin
  print_matrix_line := print_matrix_line + ' ';
end;
  print_matrix_j := print_matrix_j + 1;
end;
  writeln(print_matrix_line);
  print_matrix_i := print_matrix_i + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  A_48 := [[12, -51, 4], [6, 167, -68], [-4, 24, -41]];
  result_ := qr_decomposition(A_48);
  print_matrix(result_.q);
  print_matrix(result_.r);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

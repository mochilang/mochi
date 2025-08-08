{$mode objfpc}
program Main;
uses SysUtils;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  PI: real;
  kernel: RealArrayArray;
  deg: real;
  gamma: real;
  theta: real;
  ksize: integer;
  psi: real;
  lambd: real;
  x: real;
  sigma: real;
function to_radians(deg: real): real; forward;
function sin_taylor(x: real): real; forward;
function cos_taylor(x: real): real; forward;
function exp_taylor(x: real): real; forward;
function gabor_filter_kernel(ksize: integer; sigma: real; theta: real; lambd: real; gamma: real; psi: real): RealArrayArray; forward;
function to_radians(deg: real): real;
begin
  exit((deg * PI) / 180);
end;
function sin_taylor(x: real): real;
var
  sin_taylor_term: real;
  sin_taylor_sum: real;
  sin_taylor_i: integer;
  sin_taylor_k1: real;
  sin_taylor_k2: real;
begin
  sin_taylor_term := x;
  sin_taylor_sum := x;
  sin_taylor_i := 1;
  while sin_taylor_i < 10 do begin
  sin_taylor_k1 := 2 * Double(sin_taylor_i);
  sin_taylor_k2 := sin_taylor_k1 + 1;
  sin_taylor_term := ((-sin_taylor_term * x) * x) / (sin_taylor_k1 * sin_taylor_k2);
  sin_taylor_sum := sin_taylor_sum + sin_taylor_term;
  sin_taylor_i := sin_taylor_i + 1;
end;
  exit(sin_taylor_sum);
end;
function cos_taylor(x: real): real;
var
  cos_taylor_term: real;
  cos_taylor_sum: real;
  cos_taylor_i: integer;
  cos_taylor_k1: real;
  cos_taylor_k2: real;
begin
  cos_taylor_term := 1;
  cos_taylor_sum := 1;
  cos_taylor_i := 1;
  while cos_taylor_i < 10 do begin
  cos_taylor_k1 := (2 * Double(cos_taylor_i)) - 1;
  cos_taylor_k2 := 2 * Double(cos_taylor_i);
  cos_taylor_term := ((-cos_taylor_term * x) * x) / (cos_taylor_k1 * cos_taylor_k2);
  cos_taylor_sum := cos_taylor_sum + cos_taylor_term;
  cos_taylor_i := cos_taylor_i + 1;
end;
  exit(cos_taylor_sum);
end;
function exp_taylor(x: real): real;
var
  exp_taylor_term: real;
  exp_taylor_sum: real;
  exp_taylor_i: real;
begin
  exp_taylor_term := 1;
  exp_taylor_sum := 1;
  exp_taylor_i := 1;
  while exp_taylor_i < 20 do begin
  exp_taylor_term := (exp_taylor_term * x) / exp_taylor_i;
  exp_taylor_sum := exp_taylor_sum + exp_taylor_term;
  exp_taylor_i := exp_taylor_i + 1;
end;
  exit(exp_taylor_sum);
end;
function gabor_filter_kernel(ksize: integer; sigma: real; theta: real; lambd: real; gamma: real; psi: real): RealArrayArray;
var
  gabor_filter_kernel_size: integer;
  gabor_filter_kernel_gabor: array of RealArray;
  gabor_filter_kernel_y: integer;
  gabor_filter_kernel_row: array of real;
  gabor_filter_kernel_x: integer;
  gabor_filter_kernel_px: real;
  gabor_filter_kernel_py: real;
  gabor_filter_kernel_rad: real;
  gabor_filter_kernel_cos_theta: real;
  gabor_filter_kernel_sin_theta: real;
  gabor_filter_kernel_x_rot: real;
  gabor_filter_kernel_y_rot: real;
  gabor_filter_kernel_exponent: real;
  gabor_filter_kernel_value: real;
begin
  gabor_filter_kernel_size := ksize;
  if (gabor_filter_kernel_size mod 2) = 0 then begin
  gabor_filter_kernel_size := gabor_filter_kernel_size + 1;
end;
  gabor_filter_kernel_gabor := [];
  gabor_filter_kernel_y := 0;
  while gabor_filter_kernel_y < gabor_filter_kernel_size do begin
  gabor_filter_kernel_row := [];
  gabor_filter_kernel_x := 0;
  while gabor_filter_kernel_x < gabor_filter_kernel_size do begin
  gabor_filter_kernel_px := Double(gabor_filter_kernel_x - (gabor_filter_kernel_size div 2));
  gabor_filter_kernel_py := Double(gabor_filter_kernel_y - (gabor_filter_kernel_size div 2));
  gabor_filter_kernel_rad := to_radians(theta);
  gabor_filter_kernel_cos_theta := cos_taylor(gabor_filter_kernel_rad);
  gabor_filter_kernel_sin_theta := sin_taylor(gabor_filter_kernel_rad);
  gabor_filter_kernel_x_rot := (gabor_filter_kernel_cos_theta * gabor_filter_kernel_px) + (gabor_filter_kernel_sin_theta * gabor_filter_kernel_py);
  gabor_filter_kernel_y_rot := (-gabor_filter_kernel_sin_theta * gabor_filter_kernel_px) + (gabor_filter_kernel_cos_theta * gabor_filter_kernel_py);
  gabor_filter_kernel_exponent := -((gabor_filter_kernel_x_rot * gabor_filter_kernel_x_rot) + (((gamma * gamma) * gabor_filter_kernel_y_rot) * gabor_filter_kernel_y_rot)) / ((2 * sigma) * sigma);
  gabor_filter_kernel_value := exp_taylor(gabor_filter_kernel_exponent) * cos_taylor((((2 * PI) * gabor_filter_kernel_x_rot) / lambd) + psi);
  gabor_filter_kernel_row := concat(gabor_filter_kernel_row, [gabor_filter_kernel_value]);
  gabor_filter_kernel_x := gabor_filter_kernel_x + 1;
end;
  gabor_filter_kernel_gabor := concat(gabor_filter_kernel_gabor, [gabor_filter_kernel_row]);
  gabor_filter_kernel_y := gabor_filter_kernel_y + 1;
end;
  exit(gabor_filter_kernel_gabor);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
  kernel := gabor_filter_kernel(3, 8, 0, 10, 0, 0);
  show_list(kernel);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

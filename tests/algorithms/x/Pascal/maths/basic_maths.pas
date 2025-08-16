{$mode objfpc}{$modeswitch nestedprocvars}
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
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  arr: IntArray;
  n: integer;
  exp_: integer;
  x: integer;
  base: integer;
function pow_int(base: integer; pow_int_exp_: integer): integer; forward;
function prime_factors(n: integer): IntArray; forward;
function number_of_divisors(n: integer): integer; forward;
function sum_of_divisors(n: integer): integer; forward;
function contains(arr: IntArray; x: integer): boolean; forward;
function unique(arr: IntArray): IntArray; forward;
function euler_phi(n: integer): integer; forward;
function pow_int(base: integer; pow_int_exp_: integer): integer;
var
  pow_int_result_: integer;
  pow_int_i: integer;
begin
  pow_int_result_ := 1;
  pow_int_i := 0;
  while pow_int_i < exp_ do begin
  pow_int_result_ := pow_int_result_ * base;
  pow_int_i := pow_int_i + 1;
end;
  exit(pow_int_result_);
end;
function prime_factors(n: integer): IntArray;
var
  prime_factors_num: integer;
  prime_factors_pf: array of integer;
  prime_factors_i: integer;
begin
  if n <= 0 then begin
  panic('Only positive integers have prime factors');
end;
  prime_factors_num := n;
  prime_factors_pf := [];
  while (prime_factors_num mod 2) = 0 do begin
  prime_factors_pf := concat(prime_factors_pf, IntArray([2]));
  prime_factors_num := prime_factors_num div 2;
end;
  prime_factors_i := 3;
  while (prime_factors_i * prime_factors_i) <= prime_factors_num do begin
  while (prime_factors_num mod prime_factors_i) = 0 do begin
  prime_factors_pf := concat(prime_factors_pf, IntArray([prime_factors_i]));
  prime_factors_num := prime_factors_num div prime_factors_i;
end;
  prime_factors_i := prime_factors_i + 2;
end;
  if prime_factors_num > 2 then begin
  prime_factors_pf := concat(prime_factors_pf, IntArray([prime_factors_num]));
end;
  exit(prime_factors_pf);
end;
function number_of_divisors(n: integer): integer;
var
  number_of_divisors_num: integer;
  number_of_divisors_div_: integer;
  number_of_divisors_temp: integer;
  number_of_divisors_i: integer;
begin
  if n <= 0 then begin
  panic('Only positive numbers are accepted');
end;
  number_of_divisors_num := n;
  number_of_divisors_div_ := 1;
  number_of_divisors_temp := 1;
  while (number_of_divisors_num mod 2) = 0 do begin
  number_of_divisors_temp := number_of_divisors_temp + 1;
  number_of_divisors_num := number_of_divisors_num div 2;
end;
  number_of_divisors_div_ := number_of_divisors_div_ * number_of_divisors_temp;
  number_of_divisors_i := 3;
  while (number_of_divisors_i * number_of_divisors_i) <= number_of_divisors_num do begin
  number_of_divisors_temp := 1;
  while (number_of_divisors_num mod number_of_divisors_i) = 0 do begin
  number_of_divisors_temp := number_of_divisors_temp + 1;
  number_of_divisors_num := number_of_divisors_num div number_of_divisors_i;
end;
  number_of_divisors_div_ := number_of_divisors_div_ * number_of_divisors_temp;
  number_of_divisors_i := number_of_divisors_i + 2;
end;
  if number_of_divisors_num > 1 then begin
  number_of_divisors_div_ := number_of_divisors_div_ * 2;
end;
  exit(number_of_divisors_div_);
end;
function sum_of_divisors(n: integer): integer;
var
  sum_of_divisors_num: integer;
  sum_of_divisors_s: integer;
  sum_of_divisors_temp: integer;
  sum_of_divisors_i: integer;
begin
  if n <= 0 then begin
  panic('Only positive numbers are accepted');
end;
  sum_of_divisors_num := n;
  sum_of_divisors_s := 1;
  sum_of_divisors_temp := 1;
  while (sum_of_divisors_num mod 2) = 0 do begin
  sum_of_divisors_temp := sum_of_divisors_temp + 1;
  sum_of_divisors_num := sum_of_divisors_num div 2;
end;
  if sum_of_divisors_temp > 1 then begin
  sum_of_divisors_s := sum_of_divisors_s * ((pow_int(2, sum_of_divisors_temp) - 1) div (2 - 1));
end;
  sum_of_divisors_i := 3;
  while (sum_of_divisors_i * sum_of_divisors_i) <= sum_of_divisors_num do begin
  sum_of_divisors_temp := 1;
  while (sum_of_divisors_num mod sum_of_divisors_i) = 0 do begin
  sum_of_divisors_temp := sum_of_divisors_temp + 1;
  sum_of_divisors_num := sum_of_divisors_num div sum_of_divisors_i;
end;
  if sum_of_divisors_temp > 1 then begin
  sum_of_divisors_s := sum_of_divisors_s * ((pow_int(sum_of_divisors_i, sum_of_divisors_temp) - 1) div (sum_of_divisors_i - 1));
end;
  sum_of_divisors_i := sum_of_divisors_i + 2;
end;
  exit(sum_of_divisors_s);
end;
function contains(arr: IntArray; x: integer): boolean;
var
  contains_idx: integer;
begin
  contains_idx := 0;
  while contains_idx < Length(arr) do begin
  if arr[contains_idx] = x then begin
  exit(true);
end;
  contains_idx := contains_idx + 1;
end;
  exit(false);
end;
function unique(arr: IntArray): IntArray;
var
  unique_result_: array of integer;
  unique_idx: integer;
  unique_v: integer;
begin
  unique_result_ := [];
  unique_idx := 0;
  while unique_idx < Length(arr) do begin
  unique_v := arr[unique_idx];
  if not contains(unique_result_, unique_v) then begin
  unique_result_ := concat(unique_result_, IntArray([unique_v]));
end;
  unique_idx := unique_idx + 1;
end;
  exit(unique_result_);
end;
function euler_phi(n: integer): integer;
var
  euler_phi_s: integer;
  euler_phi_factors: IntArray;
  euler_phi_idx: integer;
  euler_phi_x: integer;
begin
  if n <= 0 then begin
  panic('Only positive numbers are accepted');
end;
  euler_phi_s := n;
  euler_phi_factors := unique(prime_factors(n));
  euler_phi_idx := 0;
  while euler_phi_idx < Length(euler_phi_factors) do begin
  euler_phi_x := euler_phi_factors[euler_phi_idx];
  euler_phi_s := (euler_phi_s div euler_phi_x) * (euler_phi_x - 1);
  euler_phi_idx := euler_phi_idx + 1;
end;
  exit(euler_phi_s);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(list_int_to_str(prime_factors(100)));
  writeln(IntToStr(number_of_divisors(100)));
  writeln(IntToStr(sum_of_divisors(100)));
  writeln(IntToStr(euler_phi(100)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

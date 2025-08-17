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
  a: integer;
  b: integer;
  denominator: integer;
  n: integer;
  number: integer;
  number1: integer;
  number2: integer;
  numerator: integer;
  p1: integer;
  p2: integer;
  x: integer;
function abs_int(x: integer): integer; forward;
function gcd_iter(a: integer; b: integer): integer; forward;
function is_prime(n: integer): boolean; forward;
function sieve_er(n: integer): IntArray; forward;
function get_prime_numbers(n: integer): IntArray; forward;
function prime_factorization(number: integer): IntArray; forward;
function greatest_prime_factor(number: integer): integer; forward;
function smallest_prime_factor(number: integer): integer; forward;
function kg_v(number1: integer; number2: integer): integer; forward;
function is_even(number: integer): boolean; forward;
function is_odd(number: integer): boolean; forward;
function goldbach(number: integer): IntArray; forward;
function get_prime(n: integer): integer; forward;
function get_primes_between(p1: integer; p2: integer): IntArray; forward;
function get_divisors(n: integer): IntArray; forward;
function is_perfect_number(number: integer): boolean; forward;
function simplify_fraction(numerator: integer; denominator: integer): IntArray; forward;
function factorial(n: integer): integer; forward;
function fib(n: integer): integer; forward;
function abs_int(x: integer): integer;
begin
  if x < 0 then begin
  exit(-x);
end;
  exit(x);
end;
function gcd_iter(a: integer; b: integer): integer;
var
  gcd_iter_x: integer;
  gcd_iter_y: integer;
  gcd_iter_t: integer;
begin
  gcd_iter_x := abs_int(a);
  gcd_iter_y := abs_int(b);
  while gcd_iter_y <> 0 do begin
  gcd_iter_t := gcd_iter_y;
  gcd_iter_y := gcd_iter_x mod gcd_iter_y;
  gcd_iter_x := gcd_iter_t;
end;
  exit(gcd_iter_x);
end;
function is_prime(n: integer): boolean;
var
  is_prime_d: integer;
begin
  if n <= 1 then begin
  exit(false);
end;
  is_prime_d := 2;
  while (is_prime_d * is_prime_d) <= n do begin
  if (n mod is_prime_d) = 0 then begin
  exit(false);
end;
  is_prime_d := is_prime_d + 1;
end;
  exit(true);
end;
function sieve_er(n: integer): IntArray;
var
  sieve_er_nums: array of integer;
  sieve_er_i: integer;
  sieve_er_idx: integer;
  sieve_er_j: integer;
  sieve_er_res: array of integer;
  sieve_er_k: integer;
  sieve_er_v: integer;
begin
  sieve_er_nums := [];
  sieve_er_i := 2;
  while sieve_er_i <= n do begin
  sieve_er_nums := concat(sieve_er_nums, IntArray([sieve_er_i]));
  sieve_er_i := sieve_er_i + 1;
end;
  sieve_er_idx := 0;
  while sieve_er_idx < Length(sieve_er_nums) do begin
  sieve_er_j := sieve_er_idx + 1;
  while sieve_er_j < Length(sieve_er_nums) do begin
  if sieve_er_nums[sieve_er_idx] <> 0 then begin
  if (sieve_er_nums[sieve_er_j] mod sieve_er_nums[sieve_er_idx]) = 0 then begin
  sieve_er_nums[sieve_er_j] := 0;
end;
end;
  sieve_er_j := sieve_er_j + 1;
end;
  sieve_er_idx := sieve_er_idx + 1;
end;
  sieve_er_res := [];
  sieve_er_k := 0;
  while sieve_er_k < Length(sieve_er_nums) do begin
  sieve_er_v := sieve_er_nums[sieve_er_k];
  if sieve_er_v <> 0 then begin
  sieve_er_res := concat(sieve_er_res, IntArray([sieve_er_v]));
end;
  sieve_er_k := sieve_er_k + 1;
end;
  exit(sieve_er_res);
end;
function get_prime_numbers(n: integer): IntArray;
var
  get_prime_numbers_ans: array of integer;
  get_prime_numbers_num: integer;
begin
  get_prime_numbers_ans := [];
  get_prime_numbers_num := 2;
  while get_prime_numbers_num <= n do begin
  if is_prime(get_prime_numbers_num) then begin
  get_prime_numbers_ans := concat(get_prime_numbers_ans, IntArray([get_prime_numbers_num]));
end;
  get_prime_numbers_num := get_prime_numbers_num + 1;
end;
  exit(get_prime_numbers_ans);
end;
function prime_factorization(number: integer): IntArray;
var
  prime_factorization_ans: array of integer;
  prime_factorization_quotient: integer;
  prime_factorization_factor: integer;
begin
  if number = 0 then begin
  exit([0]);
end;
  if number = 1 then begin
  exit([1]);
end;
  prime_factorization_ans := [];
  if is_prime(number) then begin
  prime_factorization_ans := concat(prime_factorization_ans, IntArray([number]));
  exit(prime_factorization_ans);
end;
  prime_factorization_quotient := number;
  prime_factorization_factor := 2;
  while prime_factorization_quotient <> 1 do begin
  if is_prime(prime_factorization_factor) and ((prime_factorization_quotient mod prime_factorization_factor) = 0) then begin
  prime_factorization_ans := concat(prime_factorization_ans, IntArray([prime_factorization_factor]));
  prime_factorization_quotient := prime_factorization_quotient div prime_factorization_factor;
end else begin
  prime_factorization_factor := prime_factorization_factor + 1;
end;
end;
  exit(prime_factorization_ans);
end;
function greatest_prime_factor(number: integer): integer;
var
  greatest_prime_factor_factors: IntArray;
  greatest_prime_factor_m: integer;
  greatest_prime_factor_i: integer;
begin
  greatest_prime_factor_factors := prime_factorization(number);
  greatest_prime_factor_m := greatest_prime_factor_factors[0];
  greatest_prime_factor_i := 1;
  while greatest_prime_factor_i < Length(greatest_prime_factor_factors) do begin
  if greatest_prime_factor_factors[greatest_prime_factor_i] > greatest_prime_factor_m then begin
  greatest_prime_factor_m := greatest_prime_factor_factors[greatest_prime_factor_i];
end;
  greatest_prime_factor_i := greatest_prime_factor_i + 1;
end;
  exit(greatest_prime_factor_m);
end;
function smallest_prime_factor(number: integer): integer;
var
  smallest_prime_factor_factors: IntArray;
  smallest_prime_factor_m: integer;
  smallest_prime_factor_i: integer;
begin
  smallest_prime_factor_factors := prime_factorization(number);
  smallest_prime_factor_m := smallest_prime_factor_factors[0];
  smallest_prime_factor_i := 1;
  while smallest_prime_factor_i < Length(smallest_prime_factor_factors) do begin
  if smallest_prime_factor_factors[smallest_prime_factor_i] < smallest_prime_factor_m then begin
  smallest_prime_factor_m := smallest_prime_factor_factors[smallest_prime_factor_i];
end;
  smallest_prime_factor_i := smallest_prime_factor_i + 1;
end;
  exit(smallest_prime_factor_m);
end;
function kg_v(number1: integer; number2: integer): integer;
var
  kg_v_g: integer;
begin
  if (number1 < 1) or (number2 < 1) then begin
  panic('numbers must be positive');
end;
  kg_v_g := gcd_iter(number1, number2);
  exit((number1 div kg_v_g) * number2);
end;
function is_even(number: integer): boolean;
begin
  exit((number mod 2) = 0);
end;
function is_odd(number: integer): boolean;
begin
  exit((number mod 2) <> 0);
end;
function goldbach(number: integer): IntArray;
var
  goldbach_primes: IntArray;
  goldbach_i: integer;
  goldbach_j: integer;
begin
  if not is_even(number) or (number <= 2) then begin
  panic('number must be even and > 2');
end;
  goldbach_primes := get_prime_numbers(number);
  goldbach_i := 0;
  while goldbach_i < Length(goldbach_primes) do begin
  goldbach_j := goldbach_i + 1;
  while goldbach_j < Length(goldbach_primes) do begin
  if (goldbach_primes[goldbach_i] + goldbach_primes[goldbach_j]) = number then begin
  exit([goldbach_primes[goldbach_i], goldbach_primes[goldbach_j]]);
end;
  goldbach_j := goldbach_j + 1;
end;
  goldbach_i := goldbach_i + 1;
end;
  exit([]);
end;
function get_prime(n: integer): integer;
var
  get_prime_index: integer;
  get_prime_ans: integer;
begin
  if n < 0 then begin
  panic('n must be non-negative');
end;
  get_prime_index := 0;
  get_prime_ans := 2;
  while get_prime_index < n do begin
  get_prime_index := get_prime_index + 1;
  get_prime_ans := get_prime_ans + 1;
  while not is_prime(get_prime_ans) do begin
  get_prime_ans := get_prime_ans + 1;
end;
end;
  exit(get_prime_ans);
end;
function get_primes_between(p1: integer; p2: integer): IntArray;
var
  get_primes_between_bad1: boolean;
  get_primes_between_bad2: boolean;
  get_primes_between_num: integer;
  get_primes_between_ans: array of integer;
begin
  get_primes_between_bad1 := not is_prime(p1);
  get_primes_between_bad2 := not is_prime(p2);
  if (get_primes_between_bad1 or get_primes_between_bad2) or (p1 >= p2) then begin
  panic('arguments must be prime and p1 < p2');
end;
  get_primes_between_num := p1 + 1;
  while get_primes_between_num < p2 do begin
  if is_prime(get_primes_between_num) then begin
  break;
end;
  get_primes_between_num := get_primes_between_num + 1;
end;
  get_primes_between_ans := [];
  while get_primes_between_num < p2 do begin
  get_primes_between_ans := concat(get_primes_between_ans, IntArray([get_primes_between_num]));
  get_primes_between_num := get_primes_between_num + 1;
  while get_primes_between_num < p2 do begin
  if is_prime(get_primes_between_num) then begin
  break;
end;
  get_primes_between_num := get_primes_between_num + 1;
end;
end;
  exit(get_primes_between_ans);
end;
function get_divisors(n: integer): IntArray;
var
  get_divisors_ans: array of integer;
  get_divisors_d: integer;
begin
  if n < 1 then begin
  panic('n must be >= 1');
end;
  get_divisors_ans := [];
  get_divisors_d := 1;
  while get_divisors_d <= n do begin
  if (n mod get_divisors_d) = 0 then begin
  get_divisors_ans := concat(get_divisors_ans, IntArray([get_divisors_d]));
end;
  get_divisors_d := get_divisors_d + 1;
end;
  exit(get_divisors_ans);
end;
function is_perfect_number(number: integer): boolean;
var
  is_perfect_number_divisors: IntArray;
  is_perfect_number_sum: integer;
  is_perfect_number_i: integer;
begin
  if number <= 1 then begin
  panic('number must be > 1');
end;
  is_perfect_number_divisors := get_divisors(number);
  is_perfect_number_sum := 0;
  is_perfect_number_i := 0;
  while is_perfect_number_i < (Length(is_perfect_number_divisors) - 1) do begin
  is_perfect_number_sum := is_perfect_number_sum + is_perfect_number_divisors[is_perfect_number_i];
  is_perfect_number_i := is_perfect_number_i + 1;
end;
  exit(is_perfect_number_sum = number);
end;
function simplify_fraction(numerator: integer; denominator: integer): IntArray;
var
  simplify_fraction_g: integer;
begin
  if denominator = 0 then begin
  panic('denominator cannot be zero');
end;
  simplify_fraction_g := gcd_iter(abs_int(numerator), abs_int(denominator));
  exit([numerator div simplify_fraction_g, denominator div simplify_fraction_g]);
end;
function factorial(n: integer): integer;
var
  factorial_ans: integer;
  factorial_i: integer;
begin
  if n < 0 then begin
  panic('n must be >= 0');
end;
  factorial_ans := 1;
  factorial_i := 1;
  while factorial_i <= n do begin
  factorial_ans := factorial_ans * factorial_i;
  factorial_i := factorial_i + 1;
end;
  exit(factorial_ans);
end;
function fib(n: integer): integer;
var
  fib_tmp: integer;
  fib_fib1: integer;
  fib_ans: integer;
  fib_i: integer;
begin
  if n < 0 then begin
  panic('n must be >= 0');
end;
  if n <= 1 then begin
  exit(1);
end;
  fib_tmp := 0;
  fib_fib1 := 1;
  fib_ans := 1;
  fib_i := 0;
  while fib_i < (n - 1) do begin
  fib_tmp := fib_ans;
  fib_ans := fib_ans + fib_fib1;
  fib_fib1 := fib_tmp;
  fib_i := fib_i + 1;
end;
  exit(fib_ans);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(is_prime(97), true)));
  writeln(list_int_to_str(sieve_er(20)));
  writeln(list_int_to_str(get_prime_numbers(20)));
  writeln(list_int_to_str(prime_factorization(287)));
  writeln(IntToStr(greatest_prime_factor(287)));
  writeln(IntToStr(smallest_prime_factor(287)));
  writeln(IntToStr(kg_v(8, 10)));
  writeln(list_int_to_str(goldbach(28)));
  writeln(IntToStr(get_prime(8)));
  writeln(list_int_to_str(get_primes_between(3, 23)));
  writeln(list_int_to_str(get_divisors(28)));
  writeln(LowerCase(BoolToStr(is_perfect_number(28), true)));
  writeln(list_int_to_str(simplify_fraction(10, 20)));
  writeln(IntToStr(factorial(5)));
  writeln(IntToStr(fib(10)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

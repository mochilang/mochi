{$mode objfpc}
program Main;
uses SysUtils;
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
function contains(xs: array of integer; v: integer): boolean;
var i: integer;
begin
  for i := 0 to High(xs) do begin
    if xs[i] = v then begin
      contains := true; exit;
    end;
  end;
  contains := false;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  p: integer;
  low: integer;
  mod_: integer;
  high: integer;
  keysize: integer;
  exp: integer;
  num: integer;
  base: integer;
function int_pow(base: integer; exp: integer): integer; forward;
function pow_mod(base: integer; exp: integer; mod_: integer): integer; forward;
function rand_range(low: integer; high: integer): integer; forward;
function rabin_miller(num: integer): boolean; forward;
function is_prime_low_num(num: integer): boolean; forward;
function generate_large_prime(keysize: integer): integer; forward;
function int_pow(base: integer; exp: integer): integer;
var
  int_pow_result_: integer;
  int_pow_i: integer;
begin
  int_pow_result_ := 1;
  int_pow_i := 0;
  while int_pow_i < exp do begin
  int_pow_result_ := int_pow_result_ * base;
  int_pow_i := int_pow_i + 1;
end;
  exit(int_pow_result_);
end;
function pow_mod(base: integer; exp: integer; mod_: integer): integer;
var
  pow_mod_result_: integer;
  pow_mod_b: integer;
  pow_mod_e: integer;
begin
  pow_mod_result_ := 1;
  pow_mod_b := base mod mod_;
  pow_mod_e := exp;
  while pow_mod_e > 0 do begin
  if (pow_mod_e mod 2) = 1 then begin
  pow_mod_result_ := (pow_mod_result_ * pow_mod_b) mod mod_;
end;
  pow_mod_e := pow_mod_e div 2;
  pow_mod_b := (pow_mod_b * pow_mod_b) mod mod_;
end;
  exit(pow_mod_result_);
end;
function rand_range(low: integer; high: integer): integer;
begin
  exit((_now() mod (high - low)) + low);
end;
function rabin_miller(num: integer): boolean;
var
  rabin_miller_s: integer;
  rabin_miller_t: integer;
  rabin_miller_k: integer;
  rabin_miller_a: integer;
  rabin_miller_v: integer;
  rabin_miller_i: integer;
begin
  rabin_miller_s := num - 1;
  rabin_miller_t := 0;
  while (rabin_miller_s mod 2) = 0 do begin
  rabin_miller_s := rabin_miller_s div 2;
  rabin_miller_t := rabin_miller_t + 1;
end;
  rabin_miller_k := 0;
  while rabin_miller_k < 5 do begin
  rabin_miller_a := rand_range(2, num - 1);
  rabin_miller_v := pow_mod(rabin_miller_a, rabin_miller_s, num);
  if rabin_miller_v <> 1 then begin
  rabin_miller_i := 0;
  while rabin_miller_v <> (num - 1) do begin
  if rabin_miller_i = (rabin_miller_t - 1) then begin
  exit(false);
end;
  rabin_miller_i := rabin_miller_i + 1;
  rabin_miller_v := (rabin_miller_v * rabin_miller_v) mod num;
end;
end;
  rabin_miller_k := rabin_miller_k + 1;
end;
  exit(true);
end;
function is_prime_low_num(num: integer): boolean;
var
  is_prime_low_num_low_primes: array of integer;
  is_prime_low_num_i: integer;
  is_prime_low_num_p: integer;
begin
  if num < 2 then begin
  exit(false);
end;
  is_prime_low_num_low_primes := [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997];
  if contains(is_prime_low_num_low_primes, num) then begin
  exit(true);
end;
  is_prime_low_num_i := 0;
  while is_prime_low_num_i < Length(is_prime_low_num_low_primes) do begin
  is_prime_low_num_p := is_prime_low_num_low_primes[is_prime_low_num_i];
  if (num mod is_prime_low_num_p) = 0 then begin
  exit(false);
end;
  is_prime_low_num_i := is_prime_low_num_i + 1;
end;
  exit(rabin_miller(num));
end;
function generate_large_prime(keysize: integer): integer;
var
  generate_large_prime_start: integer;
  generate_large_prime_end: integer;
  generate_large_prime_num: integer;
begin
  generate_large_prime_start := int_pow(2, keysize - 1);
  generate_large_prime_end := int_pow(2, keysize);
  while true do begin
  generate_large_prime_num := rand_range(generate_large_prime_start, generate_large_prime_end);
  if is_prime_low_num(generate_large_prime_num) then begin
  exit(generate_large_prime_num);
end;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  p := generate_large_prime(16);
  writeln('Prime number: ' + IntToStr(p));
  writeln('is_prime_low_num: ' + LowerCase(BoolToStr(is_prime_low_num(p), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

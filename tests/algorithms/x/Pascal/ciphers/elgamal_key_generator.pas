{$mode objfpc}
program Main;
uses SysUtils;
type GCD = record
  g: integer;
  x: integer;
  y: integer;
end;
type PublicKey = record
  key_size: integer;
  g: integer;
  e2: integer;
  p: integer;
end;
type PrivateKey = record
  key_size: integer;
  d: integer;
end;
type KeyPair = record
  public_key: PublicKey;
  private_key: PrivateKey;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  seed: integer;
  n: integer;
  key_size: integer;
  a: integer;
  exponent: integer;
  bits: integer;
  m: integer;
  base: integer;
  b: integer;
  min: integer;
  k: integer;
  max: integer;
  modulus: integer;
  p: integer;
function makeKeyPair(public_key: PublicKey; private_key: PrivateKey): KeyPair; forward;
function makePrivateKey(key_size: integer; d: integer): PrivateKey; forward;
function makePublicKey(key_size: integer; g: integer; e2: integer; p: integer): PublicKey; forward;
function makeGCD(g: integer; x: integer; y: integer): GCD; forward;
function rand(): integer; forward;
function rand_range(min: integer; max: integer): integer; forward;
function mod_pow(base: integer; exponent: integer; modulus: integer): integer; forward;
function extended_gcd(a: integer; b: integer): GCD; forward;
function mod_inverse(a: integer; m: integer): integer; forward;
function pow2(n: integer): integer; forward;
function is_probable_prime(n: integer; k: integer): boolean; forward;
function generate_large_prime(bits: integer): integer; forward;
function primitive_root(p: integer): integer; forward;
function generate_key(key_size: integer): KeyPair; forward;
procedure main(); forward;
function makeKeyPair(public_key: PublicKey; private_key: PrivateKey): KeyPair;
begin
  Result.public_key := public_key;
  Result.private_key := private_key;
end;
function makePrivateKey(key_size: integer; d: integer): PrivateKey;
begin
  Result.key_size := key_size;
  Result.d := d;
end;
function makePublicKey(key_size: integer; g: integer; e2: integer; p: integer): PublicKey;
begin
  Result.key_size := key_size;
  Result.g := g;
  Result.e2 := e2;
  Result.p := p;
end;
function makeGCD(g: integer; x: integer; y: integer): GCD;
begin
  Result.g := g;
  Result.x := x;
  Result.y := y;
end;
function rand(): integer;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483647;
  exit(seed);
end;
function rand_range(min: integer; max: integer): integer;
begin
  exit(min + (rand() mod ((max - min) + 1)));
end;
function mod_pow(base: integer; exponent: integer; modulus: integer): integer;
var
  mod_pow_result_: integer;
  mod_pow_b: integer;
  mod_pow_e: integer;
begin
  mod_pow_result_ := 1;
  mod_pow_b := base mod modulus;
  mod_pow_e := exponent;
  while mod_pow_e > 0 do begin
  if (mod_pow_e mod 2) = 1 then begin
  mod_pow_result_ := (mod_pow_result_ * mod_pow_b) mod modulus;
end;
  mod_pow_e := mod_pow_e div 2;
  mod_pow_b := (mod_pow_b * mod_pow_b) mod modulus;
end;
  exit(mod_pow_result_);
end;
function extended_gcd(a: integer; b: integer): GCD;
var
  extended_gcd_res: GCD;
begin
  if b = 0 then begin
  exit(makeGCD(a, 1, 0));
end;
  extended_gcd_res := extended_gcd(b, a mod b);
  exit(makeGCD(extended_gcd_res.g, extended_gcd_res.y, extended_gcd_res.x - ((a div b) * extended_gcd_res.y)));
end;
function mod_inverse(a: integer; m: integer): integer;
var
  mod_inverse_res: GCD;
  mod_inverse_r: integer;
begin
  mod_inverse_res := extended_gcd(a, m);
  if mod_inverse_res.g <> 1 then begin
  panic('inverse does not exist');
end;
  mod_inverse_r := mod_inverse_res.x mod m;
  if mod_inverse_r < 0 then begin
  exit(mod_inverse_r + m);
end;
  exit(mod_inverse_r);
end;
function pow2(n: integer): integer;
var
  pow2_r: integer;
  pow2_i: integer;
begin
  pow2_r := 1;
  pow2_i := 0;
  while pow2_i < n do begin
  pow2_r := pow2_r * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_r);
end;
function is_probable_prime(n: integer; k: integer): boolean;
var
  is_probable_prime_r: integer;
  is_probable_prime_d: integer;
  is_probable_prime_i: integer;
  is_probable_prime_a: integer;
  is_probable_prime_x: integer;
  is_probable_prime_j: integer;
  is_probable_prime_found: boolean;
begin
  if n <= 1 then begin
  exit(false);
end;
  if n <= 3 then begin
  exit(true);
end;
  if (n mod 2) = 0 then begin
  exit(false);
end;
  is_probable_prime_r := 0;
  is_probable_prime_d := n - 1;
  while (is_probable_prime_d mod 2) = 0 do begin
  is_probable_prime_d := is_probable_prime_d div 2;
  is_probable_prime_r := is_probable_prime_r + 1;
end;
  is_probable_prime_i := 0;
  while is_probable_prime_i < k do begin
  is_probable_prime_a := rand_range(2, n - 2);
  is_probable_prime_x := mod_pow(is_probable_prime_a, is_probable_prime_d, n);
  if (is_probable_prime_x = 1) or (is_probable_prime_x = (n - 1)) then begin
  is_probable_prime_i := is_probable_prime_i + 1;
  continue;
end;
  is_probable_prime_j := 1;
  is_probable_prime_found := false;
  while is_probable_prime_j < is_probable_prime_r do begin
  is_probable_prime_x := mod_pow(is_probable_prime_x, 2, n);
  if is_probable_prime_x = (n - 1) then begin
  is_probable_prime_found := true;
  break;
end;
  is_probable_prime_j := is_probable_prime_j + 1;
end;
  if not is_probable_prime_found then begin
  exit(false);
end;
  is_probable_prime_i := is_probable_prime_i + 1;
end;
  exit(true);
end;
function generate_large_prime(bits: integer): integer;
var
  generate_large_prime_min: integer;
  generate_large_prime_max: integer;
  generate_large_prime_p: integer;
begin
  generate_large_prime_min := pow2(bits - 1);
  generate_large_prime_max := pow2(bits) - 1;
  generate_large_prime_p := rand_range(generate_large_prime_min, generate_large_prime_max);
  if (generate_large_prime_p mod 2) = 0 then begin
  generate_large_prime_p := generate_large_prime_p + 1;
end;
  while not is_probable_prime(generate_large_prime_p, 5) do begin
  generate_large_prime_p := generate_large_prime_p + 2;
  if generate_large_prime_p > generate_large_prime_max then begin
  generate_large_prime_p := generate_large_prime_min + 1;
end;
end;
  exit(generate_large_prime_p);
end;
function primitive_root(p: integer): integer;
var
  primitive_root_g: integer;
begin
  while true do begin
  primitive_root_g := rand_range(3, p - 1);
  if mod_pow(primitive_root_g, 2, p) = 1 then begin
  continue;
end;
  if mod_pow(primitive_root_g, p, p) = 1 then begin
  continue;
end;
  exit(primitive_root_g);
end;
end;
function generate_key(key_size: integer): KeyPair;
var
  generate_key_p: integer;
  generate_key_e1: integer;
  generate_key_d: integer;
  generate_key_e2: integer;
  generate_key_public_key: PublicKey;
  generate_key_private_key: PrivateKey;
begin
  generate_key_p := generate_large_prime(key_size);
  generate_key_e1 := primitive_root(generate_key_p);
  generate_key_d := rand_range(3, generate_key_p - 1);
  generate_key_e2 := mod_inverse(mod_pow(generate_key_e1, generate_key_d, generate_key_p), generate_key_p);
  generate_key_public_key := makePublicKey(key_size, generate_key_e1, generate_key_e2, generate_key_p);
  generate_key_private_key := makePrivateKey(key_size, generate_key_d);
  exit(makeKeyPair(generate_key_public_key, generate_key_private_key));
end;
procedure main();
var
  main_key_size: integer;
  main_kp: KeyPair;
  main_pub: PublicKey;
  main_priv: PrivateKey;
begin
  main_key_size := 16;
  main_kp := generate_key(main_key_size);
  main_pub := main_kp.public_key;
  main_priv := main_kp.private_key;
  writeln(((((((('public key: (' + IntToStr(main_pub.key_size)) + ', ') + IntToStr(main_pub.g)) + ', ') + IntToStr(main_pub.e2)) + ', ') + IntToStr(main_pub.p)) + ')');
  writeln(((('private key: (' + IntToStr(main_priv.key_size)) + ', ') + IntToStr(main_priv.d)) + ')');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 123456789;
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

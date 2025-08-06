{$mode objfpc}
program Main;
uses SysUtils;
type Keys = record
  public_key: array of integer;
  private_key: array of integer;
end;
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
  keys_var: Keys;
  pub: array of integer;
  priv: array of integer;
  b: integer;
  bits: integer;
  exp: integer;
  min: integer;
  n: integer;
  a: integer;
  phi: integer;
  e: integer;
  x: integer;
  max: integer;
function makeKeys(public_key: IntArray; private_key: IntArray): Keys; forward;
function pow2(exp: integer): integer; forward;
function next_seed(x: integer): integer; forward;
function rand_range(min: integer; max: integer): integer; forward;
function gcd(a: integer; b: integer): integer; forward;
function mod_inverse(e: integer; phi: integer): integer; forward;
function is_prime(n: integer): boolean; forward;
function generate_prime(bits: integer): integer; forward;
function generate_key(bits: integer): Keys; forward;
function makeKeys(public_key: IntArray; private_key: IntArray): Keys;
begin
  Result.public_key := public_key;
  Result.private_key := private_key;
end;
function pow2(exp: integer): integer;
var
  pow2_res: integer;
  pow2_i: integer;
begin
  pow2_res := 1;
  pow2_i := 0;
  while pow2_i < exp do begin
  pow2_res := pow2_res * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_res);
end;
function next_seed(x: integer): integer;
begin
  exit(((x * 1103515245) + 12345) mod 2147483648);
end;
function rand_range(min: integer; max: integer): integer;
begin
  seed := next_seed(seed);
  exit(min + (seed mod (max - min)));
end;
function gcd(a: integer; b: integer): integer;
var
  gcd_x: integer;
  gcd_y: integer;
  gcd_temp: integer;
begin
  gcd_x := a;
  gcd_y := b;
  while gcd_y <> 0 do begin
  gcd_temp := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_temp;
end;
  exit(gcd_x);
end;
function mod_inverse(e: integer; phi: integer): integer;
var
  mod_inverse_t: integer;
  mod_inverse_newt: integer;
  mod_inverse_r: integer;
  mod_inverse_newr: integer;
  mod_inverse_quotient: integer;
  mod_inverse_tmp: integer;
  mod_inverse_tmp_r: integer;
begin
  mod_inverse_t := 0;
  mod_inverse_newt := 1;
  mod_inverse_r := phi;
  mod_inverse_newr := e;
  while mod_inverse_newr <> 0 do begin
  mod_inverse_quotient := mod_inverse_r div mod_inverse_newr;
  mod_inverse_tmp := mod_inverse_newt;
  mod_inverse_newt := mod_inverse_t - (mod_inverse_quotient * mod_inverse_newt);
  mod_inverse_t := mod_inverse_tmp;
  mod_inverse_tmp_r := mod_inverse_newr;
  mod_inverse_newr := mod_inverse_r - (mod_inverse_quotient * mod_inverse_newr);
  mod_inverse_r := mod_inverse_tmp_r;
end;
  if mod_inverse_r > 1 then begin
  exit(0);
end;
  if mod_inverse_t < 0 then begin
  mod_inverse_t := mod_inverse_t + phi;
end;
  exit(mod_inverse_t);
end;
function is_prime(n: integer): boolean;
var
  is_prime_i: integer;
begin
  if n < 2 then begin
  exit(false);
end;
  is_prime_i := 2;
  while (is_prime_i * is_prime_i) <= n do begin
  if (n mod is_prime_i) = 0 then begin
  exit(false);
end;
  is_prime_i := is_prime_i + 1;
end;
  exit(true);
end;
function generate_prime(bits: integer): integer;
var
  generate_prime_min: integer;
  generate_prime_max: integer;
  generate_prime_p: integer;
begin
  generate_prime_min := pow2(bits - 1);
  generate_prime_max := pow2(bits);
  generate_prime_p := rand_range(generate_prime_min, generate_prime_max);
  if (generate_prime_p mod 2) = 0 then begin
  generate_prime_p := generate_prime_p + 1;
end;
  while not is_prime(generate_prime_p) do begin
  generate_prime_p := generate_prime_p + 2;
  if generate_prime_p >= generate_prime_max then begin
  generate_prime_p := generate_prime_min + 1;
end;
end;
  exit(generate_prime_p);
end;
function generate_key(bits: integer): Keys;
var
  generate_key_p: integer;
  generate_key_q: integer;
  generate_key_n: integer;
  generate_key_phi: integer;
  generate_key_e: integer;
  generate_key_d: integer;
begin
  generate_key_p := generate_prime(bits);
  generate_key_q := generate_prime(bits);
  generate_key_n := generate_key_p * generate_key_q;
  generate_key_phi := (generate_key_p - 1) * (generate_key_q - 1);
  generate_key_e := rand_range(2, generate_key_phi);
  while gcd(generate_key_e, generate_key_phi) <> 1 do begin
  generate_key_e := generate_key_e + 1;
  if generate_key_e >= generate_key_phi then begin
  generate_key_e := 2;
end;
end;
  generate_key_d := mod_inverse(generate_key_e, generate_key_phi);
  exit(makeKeys([generate_key_n, generate_key_e], [generate_key_n, generate_key_d]));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 1;
  keys_var := generate_key(8);
  pub := keys_var.public_key;
  priv := keys_var.private_key;
  writeln(((('Public key: (' + IntToStr(pub[0])) + ', ') + IntToStr(pub[1])) + ')');
  writeln(((('Private key: (' + IntToStr(priv[0])) + ', ') + IntToStr(priv[1])) + ')');
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

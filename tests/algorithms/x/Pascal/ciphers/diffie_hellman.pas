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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  seed: integer;
  PRIME: integer;
  generator: integer;
  alice_private: integer;
  alice_public: integer;
  bob_private: integer;
  bob_public: integer;
  alice_shared: integer;
  bob_shared: integer;
  base: integer;
  n: integer;
  key: integer;
  exp: integer;
function int_to_hex(n: integer): string; forward;
function rand_int(): integer; forward;
function mod_pow(base: integer; exp: integer): integer; forward;
function is_valid_public_key(key: integer): boolean; forward;
function generate_private_key(): integer; forward;
function int_to_hex(n: integer): string;
var
  int_to_hex_digits: string;
  int_to_hex_num: integer;
  int_to_hex_res: string;
  int_to_hex_d: integer;
begin
  if n = 0 then begin
  exit('0');
end;
  int_to_hex_digits := '0123456789abcdef';
  int_to_hex_num := n;
  int_to_hex_res := '';
  while int_to_hex_num > 0 do begin
  int_to_hex_d := int_to_hex_num mod 16;
  int_to_hex_res := int_to_hex_digits[int_to_hex_d+1] + int_to_hex_res;
  int_to_hex_num := int_to_hex_num div 16;
end;
  exit(int_to_hex_res);
end;
function rand_int(): integer;
begin
  seed := ((1103515245 * seed) + 12345) mod 2147483648;
  exit(seed);
end;
function mod_pow(base: integer; exp: integer): integer;
var
  mod_pow_result_: integer;
  mod_pow_b: integer;
  mod_pow_e: integer;
begin
  mod_pow_result_ := 1;
  mod_pow_b := base mod PRIME;
  mod_pow_e := exp;
  while mod_pow_e > 0 do begin
  if (mod_pow_e mod 2) = 1 then begin
  mod_pow_result_ := (mod_pow_result_ * mod_pow_b) mod PRIME;
end;
  mod_pow_b := (mod_pow_b * mod_pow_b) mod PRIME;
  mod_pow_e := mod_pow_e div 2;
end;
  exit(mod_pow_result_);
end;
function is_valid_public_key(key: integer): boolean;
begin
  if (key < 2) or (key > (PRIME - 2)) then begin
  exit(false);
end;
  exit(mod_pow(key, (PRIME - 1) div 2) = 1);
end;
function generate_private_key(): integer;
begin
  exit((rand_int() mod (PRIME - 2)) + 2);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 123456789;
  PRIME := 23;
  generator := 5;
  alice_private := generate_private_key();
  alice_public := mod_pow(generator, alice_private);
  bob_private := generate_private_key();
  bob_public := mod_pow(generator, bob_private);
  if not is_valid_public_key(alice_public) then begin
  panic('Invalid public key');
end;
  if not is_valid_public_key(bob_public) then begin
  panic('Invalid public key');
end;
  alice_shared := mod_pow(bob_public, alice_private);
  bob_shared := mod_pow(alice_public, bob_private);
  writeln(int_to_hex(alice_shared));
  writeln(int_to_hex(bob_shared));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

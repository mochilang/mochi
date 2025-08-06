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
  base: integer;
  exp: integer;
  mod_: integer;
  n: integer;
  allow_probable: boolean;
function mod_pow(base: integer; exp: integer; mod_: integer): integer; forward;
function miller_rabin(n: integer; allow_probable: boolean): boolean; forward;
function mod_pow(base: integer; exp: integer; mod_: integer): integer;
var
  mod_pow_result_: integer;
  mod_pow_b: integer;
  mod_pow_e: integer;
begin
  mod_pow_result_ := 1;
  mod_pow_b := base mod mod_;
  mod_pow_e := exp;
  while mod_pow_e > 0 do begin
  if (mod_pow_e mod 2) = 1 then begin
  mod_pow_result_ := (mod_pow_result_ * mod_pow_b) mod mod_;
end;
  mod_pow_b := (mod_pow_b * mod_pow_b) mod mod_;
  mod_pow_e := mod_pow_e div 2;
end;
  exit(mod_pow_result_);
end;
function miller_rabin(n: integer; allow_probable: boolean): boolean;
var
  miller_rabin_last: integer;
  miller_rabin_limit: integer;
  miller_rabin_bounds: array of integer;
  miller_rabin_primes: array of integer;
  miller_rabin_i: integer;
  miller_rabin_plist_len: integer;
  miller_rabin_d: integer;
  miller_rabin_s: integer;
  miller_rabin_j: integer;
  miller_rabin_prime: integer;
  miller_rabin_x: integer;
  miller_rabin_pr: boolean;
  miller_rabin_r: integer;
begin
  if n = 2 then begin
  exit(true);
end;
  if (n < 2) or ((n mod 2) = 0) then begin
  exit(false);
end;
  if n > 5 then begin
  miller_rabin_last := n mod 10;
  if not ((((miller_rabin_last = 1) or (miller_rabin_last = 3)) or (miller_rabin_last = 7)) or (miller_rabin_last = 9)) then begin
  exit(false);
end;
end;
  miller_rabin_limit := 3825123056546413051;
  if (n > miller_rabin_limit) and not allow_probable then begin
  panic('Warning: upper bound of deterministic test is exceeded. Pass allow_probable=true to allow probabilistic test.');
end;
  miller_rabin_bounds := [2047, 1373653, 25326001, 3215031751, 2152302898747, 3474749660383, 341550071728321, miller_rabin_limit];
  miller_rabin_primes := [2, 3, 5, 7, 11, 13, 17, 19];
  miller_rabin_i := 0;
  miller_rabin_plist_len := Length(miller_rabin_primes);
  while miller_rabin_i < Length(miller_rabin_bounds) do begin
  if n < miller_rabin_bounds[miller_rabin_i] then begin
  miller_rabin_plist_len := miller_rabin_i + 1;
  miller_rabin_i := Length(miller_rabin_bounds);
end else begin
  miller_rabin_i := miller_rabin_i + 1;
end;
end;
  miller_rabin_d := n - 1;
  miller_rabin_s := 0;
  while (miller_rabin_d mod 2) = 0 do begin
  miller_rabin_d := miller_rabin_d div 2;
  miller_rabin_s := miller_rabin_s + 1;
end;
  miller_rabin_j := 0;
  while miller_rabin_j < miller_rabin_plist_len do begin
  miller_rabin_prime := miller_rabin_primes[miller_rabin_j];
  miller_rabin_x := mod_pow(miller_rabin_prime, miller_rabin_d, n);
  miller_rabin_pr := false;
  if (miller_rabin_x = 1) or (miller_rabin_x = (n - 1)) then begin
  miller_rabin_pr := true;
end else begin
  miller_rabin_r := 1;
  while (miller_rabin_r < miller_rabin_s) and not miller_rabin_pr do begin
  miller_rabin_x := (miller_rabin_x * miller_rabin_x) mod n;
  if miller_rabin_x = (n - 1) then begin
  miller_rabin_pr := true;
end;
  miller_rabin_r := miller_rabin_r + 1;
end;
end;
  if not miller_rabin_pr then begin
  exit(false);
end;
  miller_rabin_j := miller_rabin_j + 1;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(miller_rabin(561, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(563, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(838201, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(838207, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(17316001, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(17316017, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(3078386641, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(3078386653, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(1713045574801, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(1713045574819, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(2779799728307, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(2779799728327, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(113850023909441, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(113850023909527, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(1275041018848804351, false), true)));
  writeln(LowerCase(BoolToStr(miller_rabin(1275041018848804391, false), true)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

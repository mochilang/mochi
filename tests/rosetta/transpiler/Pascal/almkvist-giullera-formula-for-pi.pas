{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function bigTrim(a: IntArray): IntArray; forward;
function bigFromInt(x: integer): IntArray; forward;
function bigCmp(a: IntArray; b: IntArray): integer; forward;
function bigAdd(a: IntArray; b: IntArray): IntArray; forward;
function bigSub(a: IntArray; b: IntArray): IntArray; forward;
function bigMulSmall(a: IntArray; m: integer): IntArray; forward;
function bigMulBig(a: IntArray; b: IntArray): IntArray; forward;
function bigMulPow10(a: IntArray; k: integer): IntArray; forward;
function bigDivSmall(a: IntArray; m: integer): IntArray; forward;
function bigToString(a: IntArray): string; forward;
function repeat(ch: string; n: integer): string; forward;
function sortInts(xs: IntArray): IntArray; forward;
function primesUpTo(n: integer): IntArray; forward;
function factorialExp(n: integer; primes: IntArray): specialize TFPGMap<string, integer>; forward;
function factorSmall(x: integer; primes: IntArray): specialize TFPGMap<string, integer>; forward;
function computeIP(n: integer; primes: IntArray): IntArray; forward;
function formatTerm(ip: IntArray; pw: integer): string; forward;
function bigAbsDiff(a: IntArray; b: IntArray): IntArray; forward;
procedure main(); forward;
function bigTrim(a: IntArray): IntArray;
var
  bigTrim_n: integer;
begin
  bigTrim_n := Length(a);
  while (bigTrim_n > 1) and (a[bigTrim_n - 1] = 0) do begin
  a := copy(a, 0, (bigTrim_n - 1 - (0)));
  bigTrim_n := bigTrim_n - 1;
end;
  exit(a);
end;
function bigFromInt(x: integer): IntArray;
var
  bigFromInt_digits: array of integer;
  bigFromInt_n: integer;
begin
  if x = 0 then begin
  exit([0]);
end;
  bigFromInt_digits := [];
  bigFromInt_n := x;
  while bigFromInt_n > 0 do begin
  bigFromInt_digits := concat(bigFromInt_digits, [bigFromInt_n mod 10]);
  bigFromInt_n := bigFromInt_n div 10;
end;
  exit(bigFromInt_digits);
end;
function bigCmp(a: IntArray; b: IntArray): integer;
var
  bigCmp_i: integer;
begin
  if Length(a) > Length(b) then begin
  exit(1);
end;
  if Length(a) < Length(b) then begin
  exit(-1);
end;
  bigCmp_i := Length(a) - 1;
  while bigCmp_i >= 0 do begin
  if a[bigCmp_i] > b[bigCmp_i] then begin
  exit(1);
end;
  if a[bigCmp_i] < b[bigCmp_i] then begin
  exit(-1);
end;
  bigCmp_i := bigCmp_i - 1;
end;
  exit(0);
end;
function bigAdd(a: IntArray; b: IntArray): IntArray;
var
  bigAdd_res: array of integer;
  bigAdd_carry: integer;
  bigAdd_i: integer;
  bigAdd_av: integer;
  bigAdd_bv: integer;
  bigAdd_s: integer;
begin
  bigAdd_res := [];
  bigAdd_carry := 0;
  bigAdd_i := 0;
  while ((bigAdd_i < Length(a)) or (bigAdd_i < Length(b))) or (bigAdd_carry > 0) do begin
  bigAdd_av := 0;
  if bigAdd_i < Length(a) then begin
  bigAdd_av := a[bigAdd_i];
end;
  bigAdd_bv := 0;
  if bigAdd_i < Length(b) then begin
  bigAdd_bv := b[bigAdd_i];
end;
  bigAdd_s := (bigAdd_av + bigAdd_bv) + bigAdd_carry;
  bigAdd_res := concat(bigAdd_res, [bigAdd_s mod 10]);
  bigAdd_carry := bigAdd_s div 10;
  bigAdd_i := bigAdd_i + 1;
end;
  exit(bigTrim(bigAdd_res));
end;
function bigSub(a: IntArray; b: IntArray): IntArray;
var
  bigSub_res: array of integer;
  bigSub_borrow: integer;
  bigSub_i: integer;
  bigSub_av: integer;
  bigSub_bv: integer;
  bigSub_diff: integer;
begin
  bigSub_res := [];
  bigSub_borrow := 0;
  bigSub_i := 0;
  while bigSub_i < Length(a) do begin
  bigSub_av := a[bigSub_i];
  bigSub_bv := 0;
  if bigSub_i < Length(b) then begin
  bigSub_bv := b[bigSub_i];
end;
  bigSub_diff := (bigSub_av - bigSub_bv) - bigSub_borrow;
  if bigSub_diff < 0 then begin
  bigSub_diff := bigSub_diff + 10;
  bigSub_borrow := 1;
end else begin
  bigSub_borrow := 0;
end;
  bigSub_res := concat(bigSub_res, [bigSub_diff]);
  bigSub_i := bigSub_i + 1;
end;
  exit(bigTrim(bigSub_res));
end;
function bigMulSmall(a: IntArray; m: integer): IntArray;
var
  bigMulSmall_res: array of integer;
  bigMulSmall_carry: integer;
  bigMulSmall_i: integer;
  bigMulSmall_prod: integer;
begin
  if m = 0 then begin
  exit([0]);
end;
  bigMulSmall_res := [];
  bigMulSmall_carry := 0;
  bigMulSmall_i := 0;
  while bigMulSmall_i < Length(a) do begin
  bigMulSmall_prod := (a[bigMulSmall_i] * m) + bigMulSmall_carry;
  bigMulSmall_res := concat(bigMulSmall_res, [bigMulSmall_prod mod 10]);
  bigMulSmall_carry := bigMulSmall_prod div 10;
  bigMulSmall_i := bigMulSmall_i + 1;
end;
  while bigMulSmall_carry > 0 do begin
  bigMulSmall_res := concat(bigMulSmall_res, [bigMulSmall_carry mod 10]);
  bigMulSmall_carry := bigMulSmall_carry div 10;
end;
  exit(bigTrim(bigMulSmall_res));
end;
function bigMulBig(a: IntArray; b: IntArray): IntArray;
var
  bigMulBig_res: array of integer;
  bigMulBig_i: integer;
  bigMulBig_carry: integer;
  bigMulBig_j: integer;
  bigMulBig_idx: integer;
  bigMulBig_prod: integer;
begin
  bigMulBig_res := [];
  bigMulBig_i := 0;
  while bigMulBig_i < (Length(a) + Length(b)) do begin
  bigMulBig_res := concat(bigMulBig_res, [0]);
  bigMulBig_i := bigMulBig_i + 1;
end;
  bigMulBig_i := 0;
  while bigMulBig_i < Length(a) do begin
  bigMulBig_carry := 0;
  bigMulBig_j := 0;
  while bigMulBig_j < Length(b) do begin
  bigMulBig_idx := bigMulBig_i + bigMulBig_j;
  bigMulBig_prod := (bigMulBig_res[bigMulBig_idx] + (a[bigMulBig_i] * b[bigMulBig_j])) + bigMulBig_carry;
  bigMulBig_res[bigMulBig_idx] := bigMulBig_prod mod 10;
  bigMulBig_carry := bigMulBig_prod div 10;
  bigMulBig_j := bigMulBig_j + 1;
end;
  bigMulBig_idx := bigMulBig_i + Length(b);
  while bigMulBig_carry > 0 do begin
  bigMulBig_prod := bigMulBig_res[bigMulBig_idx] + bigMulBig_carry;
  bigMulBig_res[bigMulBig_idx] := bigMulBig_prod mod 10;
  bigMulBig_carry := bigMulBig_prod div 10;
  bigMulBig_idx := bigMulBig_idx + 1;
end;
  bigMulBig_i := bigMulBig_i + 1;
end;
  exit(bigTrim(bigMulBig_res));
end;
function bigMulPow10(a: IntArray; k: integer): IntArray;
var
  bigMulPow10_i: integer;
begin
  bigMulPow10_i := 0;
  while bigMulPow10_i < k do begin
  a := concat([0], a);
  bigMulPow10_i := bigMulPow10_i + 1;
end;
  exit(a);
end;
function bigDivSmall(a: IntArray; m: integer): IntArray;
var
  bigDivSmall_res: array of integer;
  bigDivSmall_rem: integer;
  bigDivSmall_i: integer;
  bigDivSmall_cur: integer;
  bigDivSmall_q: integer;
begin
  bigDivSmall_res := [];
  bigDivSmall_rem := 0;
  bigDivSmall_i := Length(a) - 1;
  while bigDivSmall_i >= 0 do begin
  bigDivSmall_cur := (bigDivSmall_rem * 10) + a[bigDivSmall_i];
  bigDivSmall_q := bigDivSmall_cur div m;
  bigDivSmall_rem := bigDivSmall_cur mod m;
  bigDivSmall_res := concat([bigDivSmall_q], bigDivSmall_res);
  bigDivSmall_i := bigDivSmall_i - 1;
end;
  exit(bigTrim(bigDivSmall_res));
end;
function bigToString(a: IntArray): string;
var
  bigToString_s: string;
  bigToString_i: integer;
begin
  bigToString_s := '';
  bigToString_i := Length(a) - 1;
  while bigToString_i >= 0 do begin
  bigToString_s := bigToString_s + IntToStr(a[bigToString_i]);
  bigToString_i := bigToString_i - 1;
end;
  exit(bigToString_s);
end;
function repeat(ch: string; n: integer): string;
var
  repeat_s: string;
  repeat_i: integer;
begin
  repeat_s := '';
  repeat_i := 0;
  while repeat_i < n do begin
  repeat_s := repeat_s + ch;
  repeat_i := repeat_i + 1;
end;
  exit(repeat_s);
end;
function sortInts(xs: IntArray): IntArray;
var
  sortInts_res: array of integer;
  sortInts_tmp: array of integer;
  sortInts_min: integer;
  sortInts_idx: integer;
  sortInts_i: integer;
  sortInts_out: array of integer;
  sortInts_j: integer;
begin
  sortInts_res := [];
  sortInts_tmp := xs;
  while Length(sortInts_tmp) > 0 do begin
  sortInts_min := sortInts_tmp[0];
  sortInts_idx := 0;
  sortInts_i := 1;
  while sortInts_i < Length(sortInts_tmp) do begin
  if sortInts_tmp[sortInts_i] < sortInts_min then begin
  sortInts_min := sortInts_tmp[sortInts_i];
  sortInts_idx := sortInts_i;
end;
  sortInts_i := sortInts_i + 1;
end;
  sortInts_res := concat(sortInts_res, [sortInts_min]);
  sortInts_out := [];
  sortInts_j := 0;
  while sortInts_j < Length(sortInts_tmp) do begin
  if sortInts_j <> sortInts_idx then begin
  sortInts_out := concat(sortInts_out, [sortInts_tmp[sortInts_j]]);
end;
  sortInts_j := sortInts_j + 1;
end;
  sortInts_tmp := sortInts_out;
end;
  exit(sortInts_res);
end;
function primesUpTo(n: integer): IntArray;
var
  primesUpTo_sieve: array of boolean;
  primesUpTo_i: integer;
  primesUpTo_p: integer;
  primesUpTo_m: integer;
  primesUpTo_res: array of integer;
  primesUpTo_x: integer;
begin
  primesUpTo_sieve := [];
  primesUpTo_i := 0;
  while primesUpTo_i <= n do begin
  primesUpTo_sieve := concat(primesUpTo_sieve, [true]);
  primesUpTo_i := primesUpTo_i + 1;
end;
  primesUpTo_p := 2;
  while (primesUpTo_p * primesUpTo_p) <= n do begin
  if primesUpTo_sieve[primesUpTo_p] then begin
  primesUpTo_m := primesUpTo_p * primesUpTo_p;
  while primesUpTo_m <= n do begin
  primesUpTo_sieve[primesUpTo_m] := false;
  primesUpTo_m := primesUpTo_m + primesUpTo_p;
end;
end;
  primesUpTo_p := primesUpTo_p + 1;
end;
  primesUpTo_res := [];
  primesUpTo_x := 2;
  while primesUpTo_x <= n do begin
  if primesUpTo_sieve[primesUpTo_x] then begin
  primesUpTo_res := concat(primesUpTo_res, [primesUpTo_x]);
end;
  primesUpTo_x := primesUpTo_x + 1;
end;
  exit(primesUpTo_res);
end;
function factorialExp(n: integer; primes: IntArray): specialize TFPGMap<string, integer>;
var
  factorialExp_m: specialize TFPGMap<string, integer>;
  factorialExp_p: integer;
  factorialExp_t: integer;
  factorialExp_e: integer;
begin
  for factorialExp_p in primes do begin
  if factorialExp_p > n then begin
  break;
end;
  factorialExp_t := n;
  factorialExp_e := 0;
  while factorialExp_t > 0 do begin
  factorialExp_t := factorialExp_t div factorialExp_p;
  factorialExp_e := factorialExp_e + factorialExp_t;
end;
  factorialExp_m.AddOrSetData(IntToStr(factorialExp_p), factorialExp_e);
end;
  exit(factorialExp_m);
end;
function factorSmall(x: integer; primes: IntArray): specialize TFPGMap<string, integer>;
var
  factorSmall_f: specialize TFPGMap<string, integer>;
  factorSmall_n: integer;
  factorSmall_p: integer;
  factorSmall_c: integer;
begin
  factorSmall_n := x;
  for factorSmall_p in primes do begin
  if (factorSmall_p * factorSmall_p) > factorSmall_n then begin
  break;
end;
  factorSmall_c := 0;
  while (factorSmall_n mod factorSmall_p) = 0 do begin
  factorSmall_c := factorSmall_c + 1;
  factorSmall_n := factorSmall_n div factorSmall_p;
end;
  if factorSmall_c > 0 then begin
  factorSmall_f.AddOrSetData(IntToStr(factorSmall_p), factorSmall_c);
end;
end;
  if factorSmall_n > 1 then begin
  factorSmall_f.AddOrSetData(IntToStr(factorSmall_n), factorSmall_f.get + 1);
end;
  exit(factorSmall_f);
end;
function computeIP(n: integer; primes: IntArray): IntArray;
var
  computeIP_exps: specialize TFPGMap<string, integer>;
  computeIP_fn: specialize TFPGMap<string, integer>;
  computeIP_k: integer;
  computeIP_t2: integer;
  computeIP_ft2: specialize TFPGMap<string, integer>;
  computeIP_keys: array of integer;
  computeIP_res: IntArray;
  computeIP_p: integer;
  computeIP_e: integer;
  computeIP_i: integer;
begin
  computeIP_exps := factorialExp(6 * n, primes);
  computeIP_fn := factorialExp(n, primes);
  for computeIP_k in computeIP_fn do begin
  computeIP_exps.AddOrSetData(computeIP_k, computeIP_exps.get - (6 * computeIP_fn[computeIP_k]));
end;
  computeIP_exps.AddOrSetData('2', computeIP_exps.get + 5);
  computeIP_t2 := (((532 * n) * n) + (126 * n)) + 9;
  computeIP_ft2 := factorSmall(computeIP_t2, primes);
  for computeIP_k in computeIP_ft2 do begin
  computeIP_exps.AddOrSetData(computeIP_k, computeIP_exps.get + computeIP_ft2[computeIP_k]);
end;
  computeIP_exps.AddOrSetData('3', computeIP_exps.get - 1);
  computeIP_keys := [];
  for computeIP_k in computeIP_exps do begin
  computeIP_keys := concat(computeIP_keys, [Trunc(computeIP_k)]);
end;
  computeIP_keys := sortInts(computeIP_keys);
  computeIP_res := bigFromInt(1);
  for computeIP_p in computeIP_keys do begin
  computeIP_e := computeIP_exps[IntToStr(computeIP_p)];
  computeIP_i := 0;
  while computeIP_i < computeIP_e do begin
  computeIP_res := bigMulSmall(computeIP_res, computeIP_p);
  computeIP_i := computeIP_i + 1;
end;
end;
  exit(computeIP_res);
end;
function formatTerm(ip: IntArray; pw: integer): string;
var
  formatTerm_s: string;
  formatTerm_frac: string;
  formatTerm_intpart: string;
begin
  formatTerm_s := bigToString(ip);
  if pw >= Length(formatTerm_s) then begin
  formatTerm_frac := repeat('0', pw - Length(formatTerm_s)) + formatTerm_s;
  if Length(formatTerm_frac) < 33 then begin
  formatTerm_frac := formatTerm_frac + repeat('0', 33 - Length(formatTerm_frac));
end;
  exit('0.' + copy(formatTerm_frac, 0+1, (33 - (0))));
end;
  formatTerm_intpart := copy(formatTerm_s, 0+1, (Length(formatTerm_s) - pw - (0)));
  formatTerm_frac := copy(formatTerm_s, Length(formatTerm_s) - pw+1, (Length(formatTerm_s) - (Length(formatTerm_s) - pw)));
  if Length(formatTerm_frac) < 33 then begin
  formatTerm_frac := formatTerm_frac + repeat('0', 33 - Length(formatTerm_frac));
end;
  exit((formatTerm_intpart + '.') + copy(formatTerm_frac, 0+1, (33 - (0))));
end;
function bigAbsDiff(a: IntArray; b: IntArray): IntArray;
begin
  if bigCmp(a, b) >= 0 then begin
  exit(bigSub(a, b));
end;
  exit(bigSub(b, a));
end;
procedure main();
var
  main_primes: IntArray;
  main_line: string;
  main_sum: IntArray;
  main_prev: IntArray;
  main_denomPow: integer;
  main_n: integer;
  main_ip: IntArray;
  main_pw: integer;
  main_termStr: string;
  main_ipStr: string;
  main_pwStr: string;
  main_padTerm: string;
  main_diff: IntArray;
  main_precision: integer;
  main_target: IntArray;
  main_low: IntArray;
  main_high: IntArray;
  main_mid: IntArray;
  main_prod: IntArray;
  main_piInt: array of integer;
  main_piStr: string;
  main_out: string;
begin
  main_primes := primesUpTo(2000);
  writeln('N                               Integer Portion  Pow  Nth Term (33 dp)');
  main_line := repeat('-', 89);
  writeln(main_line);
  main_sum := bigFromInt(0);
  main_prev := bigFromInt(0);
  main_denomPow := 0;
  main_n := 0;
  while true do begin
  main_ip := computeIP(main_n, main_primes);
  main_pw := (6 * main_n) + 3;
  if main_pw > main_denomPow then begin
  main_sum := bigMulPow10(main_sum, main_pw - main_denomPow);
  main_prev := bigMulPow10(main_prev, main_pw - main_denomPow);
  main_denomPow := main_pw;
end;
  if main_n < 10 then begin
  main_termStr := formatTerm(main_ip, main_pw);
  main_ipStr := bigToString(main_ip);
  while Length(main_ipStr) < 44 do begin
  main_ipStr := ' ' + main_ipStr;
end;
  main_pwStr := IntToStr(-main_pw);
  while Length(main_pwStr) < 3 do begin
  main_pwStr := ' ' + main_pwStr;
end;
  main_padTerm := main_termStr;
  while Length(main_padTerm) < 35 do begin
  main_padTerm := main_padTerm + ' ';
end;
  writeln((((((IntToStr(main_n) + '  ') + main_ipStr) + '  ') + main_pwStr) + '  ') + main_padTerm);
end;
  main_sum := bigAdd(main_sum, main_ip);
  main_diff := bigAbsDiff(main_sum, main_prev);
  if (main_denomPow >= 70) and (bigCmp(main_diff, bigMulPow10(bigFromInt(1), main_denomPow - 70)) < 0) then begin
  break;
end;
  main_prev := main_sum;
  main_n := main_n + 1;
end;
  main_precision := 70;
  main_target := bigMulPow10(bigFromInt(1), main_denomPow + (2 * main_precision));
  main_low := bigFromInt(0);
  main_high := bigMulPow10(bigFromInt(1), main_precision + 1);
  while bigCmp(main_low, bigSub(main_high, bigFromInt(1))) < 0 do begin
  main_mid := bigDivSmall(bigAdd(main_low, main_high), 2);
  main_prod := bigMulBig(bigMulBig(main_mid, main_mid), main_sum);
  if bigCmp(main_prod, main_target) <= 0 then begin
  main_low := main_mid;
end else begin
  main_high := bigSub(main_mid, bigFromInt(1));
end;
end;
  main_piInt := main_low;
  main_piStr := bigToString(main_piInt);
  if Length(main_piStr) <= main_precision then begin
  main_piStr := repeat('0', (main_precision - Length(main_piStr)) + 1) + main_piStr;
end;
  main_out := (copy(main_piStr, 0+1, (Length(main_piStr) - main_precision - (0))) + '.') + copy(main_piStr, Length(main_piStr) - main_precision+1, (Length(main_piStr) - (Length(main_piStr) - main_precision)));
  writeln('');
  writeln('Pi to 70 decimal places is:');
  writeln(main_out);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

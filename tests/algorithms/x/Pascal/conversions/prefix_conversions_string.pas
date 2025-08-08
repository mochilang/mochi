{$mode objfpc}
program Main;
uses SysUtils;
type Prefix = record
  name: string;
  exp: integer;
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
  si_positive: array of Prefix;
  si_negative: array of Prefix;
  binary_prefixes: array of Prefix;
  base: real;
  value: real;
  exp: integer;
function makePrefix(name: string; exp: integer): Prefix; forward;
function pow(base: real; exp: integer): real; forward;
function add_si_prefix(value: real): string; forward;
function add_binary_prefix(value: real): string; forward;
function makePrefix(name: string; exp: integer): Prefix;
begin
  Result.name := name;
  Result.exp := exp;
end;
function pow(base: real; exp: integer): real;
var
  pow_result_: real;
  pow_e: integer;
  pow_i: integer;
begin
  pow_result_ := 1;
  pow_e := exp;
  if pow_e < 0 then begin
  pow_e := -pow_e;
  pow_i := 0;
  while pow_i < pow_e do begin
  pow_result_ := pow_result_ * base;
  pow_i := pow_i + 1;
end;
  exit(1 / pow_result_);
end;
  pow_i := 0;
  while pow_i < pow_e do begin
  pow_result_ := pow_result_ * base;
  pow_i := pow_i + 1;
end;
  exit(pow_result_);
end;
function add_si_prefix(value: real): string;
var
  add_si_prefix_prefixes: array of Prefix;
  add_si_prefix_i: integer;
  add_si_prefix_p: Prefix;
  add_si_prefix_num: real;
begin
  if value > 0 then begin
  add_si_prefix_prefixes := si_positive;
end else begin
  add_si_prefix_prefixes := si_negative;
end;
  add_si_prefix_i := 0;
  while add_si_prefix_i < Length(add_si_prefix_prefixes) do begin
  add_si_prefix_p := add_si_prefix_prefixes[add_si_prefix_i];
  add_si_prefix_num := value / pow(10, add_si_prefix_p.exp);
  if add_si_prefix_num > 1 then begin
  exit((FloatToStr(add_si_prefix_num) + ' ') + add_si_prefix_p.name);
end;
  add_si_prefix_i := add_si_prefix_i + 1;
end;
  exit(FloatToStr(value));
end;
function add_binary_prefix(value: real): string;
var
  add_binary_prefix_i: integer;
  add_binary_prefix_p: Prefix;
  add_binary_prefix_num: real;
begin
  add_binary_prefix_i := 0;
  while add_binary_prefix_i < Length(binary_prefixes) do begin
  add_binary_prefix_p := binary_prefixes[add_binary_prefix_i];
  add_binary_prefix_num := value / pow(2, add_binary_prefix_p.exp);
  if add_binary_prefix_num > 1 then begin
  exit((FloatToStr(add_binary_prefix_num) + ' ') + add_binary_prefix_p.name);
end;
  add_binary_prefix_i := add_binary_prefix_i + 1;
end;
  exit(FloatToStr(value));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  si_positive := [makePrefix('yotta', 24), makePrefix('zetta', 21), makePrefix('exa', 18), makePrefix('peta', 15), makePrefix('tera', 12), makePrefix('giga', 9), makePrefix('mega', 6), makePrefix('kilo', 3), makePrefix('hecto', 2), makePrefix('deca', 1)];
  si_negative := [makePrefix('deci', -1), makePrefix('centi', -2), makePrefix('milli', -3), makePrefix('micro', -6), makePrefix('nano', -9), makePrefix('pico', -12), makePrefix('femto', -15), makePrefix('atto', -18), makePrefix('zepto', -21), makePrefix('yocto', -24)];
  binary_prefixes := [makePrefix('yotta', 80), makePrefix('zetta', 70), makePrefix('exa', 60), makePrefix('peta', 50), makePrefix('tera', 40), makePrefix('giga', 30), makePrefix('mega', 20), makePrefix('kilo', 10)];
  writeln(add_si_prefix(10000));
  writeln(add_si_prefix(0.005));
  writeln(add_binary_prefix(65536));
  writeln(add_binary_prefix(512));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

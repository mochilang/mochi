{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
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
  a: integer;
  b: integer;
  nums: IntArray;
function gcd(a: integer; b: integer): integer; forward;
function get_greatest_common_divisor(nums: IntArray): integer; forward;
function gcd(a: integer; b: integer): integer;
var
  gcd_x: integer;
  gcd_y: integer;
  gcd_r: integer;
begin
  gcd_x := a;
  gcd_y := b;
  while gcd_y <> 0 do begin
  gcd_r := gcd_x mod gcd_y;
  gcd_x := gcd_y;
  gcd_y := gcd_r;
end;
  if gcd_x < 0 then begin
  exit(-gcd_x);
end;
  exit(gcd_x);
end;
function get_greatest_common_divisor(nums: IntArray): integer;
var
  get_greatest_common_divisor_g: integer;
  get_greatest_common_divisor_i: integer;
  get_greatest_common_divisor_n: integer;
begin
  if Length(nums) = 0 then begin
  panic('at least one number is required');
end;
  get_greatest_common_divisor_g := nums[0];
  if get_greatest_common_divisor_g <= 0 then begin
  panic('numbers must be integer and greater than zero');
end;
  get_greatest_common_divisor_i := 1;
  while get_greatest_common_divisor_i < Length(nums) do begin
  get_greatest_common_divisor_n := nums[get_greatest_common_divisor_i];
  if get_greatest_common_divisor_n <= 0 then begin
  panic('numbers must be integer and greater than zero');
end;
  get_greatest_common_divisor_g := gcd(get_greatest_common_divisor_g, get_greatest_common_divisor_n);
  get_greatest_common_divisor_i := get_greatest_common_divisor_i + 1;
end;
  exit(get_greatest_common_divisor_g);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(IntToStr(get_greatest_common_divisor([18, 45])));
  writeln(IntToStr(get_greatest_common_divisor([23, 37])));
  writeln(IntToStr(get_greatest_common_divisor([2520, 8350])));
  writeln(IntToStr(get_greatest_common_divisor([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function intSqrt(x: integer): integer; forward;
function sumRecip(n: integer): integer; forward;
procedure main(); forward;
function intSqrt(x: integer): integer;
var
  intSqrt_left: integer;
  intSqrt_right: integer;
  intSqrt_ans: integer;
  intSqrt_mid: integer;
  intSqrt_sq: integer;
begin
  if x < 2 then begin
  exit(x);
end;
  intSqrt_left := 1;
  intSqrt_right := x div 2;
  intSqrt_ans := 0;
  while intSqrt_left <= intSqrt_right do begin
  intSqrt_mid := intSqrt_left + ((intSqrt_right - intSqrt_left) div 2);
  intSqrt_sq := intSqrt_mid * intSqrt_mid;
  if intSqrt_sq = x then begin
  exit(intSqrt_mid);
end;
  if intSqrt_sq < x then begin
  intSqrt_left := intSqrt_mid + 1;
  intSqrt_ans := intSqrt_mid;
end else begin
  intSqrt_right := intSqrt_mid - 1;
end;
end;
  exit(intSqrt_ans);
end;
function sumRecip(n: integer): integer;
var
  sumRecip_s: integer;
  sumRecip_limit: integer;
  sumRecip_f: integer;
  sumRecip_f2: integer;
begin
  sumRecip_s := 1;
  sumRecip_limit := intSqrt(n);
  sumRecip_f := 2;
  while sumRecip_f <= sumRecip_limit do begin
  if (n mod sumRecip_f) = 0 then begin
  sumRecip_s := sumRecip_s + (n div sumRecip_f);
  sumRecip_f2 := n div sumRecip_f;
  if sumRecip_f2 <> sumRecip_f then begin
  sumRecip_s := sumRecip_s + sumRecip_f;
end;
end;
  sumRecip_f := sumRecip_f + 1;
end;
  exit(sumRecip_s);
end;
procedure main();
var
  main_nums: array of integer;
  main_n: integer;
  main_s: integer;
  main_val: integer;
  main_perfect: string;
begin
  main_nums := [6, 28, 120, 496, 672, 8128, 30240, 32760, 523776];
  for main_n in main_nums do begin
  main_s := sumRecip(main_n);
  if (main_s mod main_n) = 0 then begin
  main_val := main_s div main_n;
  main_perfect := '';
  if main_val = 1 then begin
  main_perfect := 'perfect!';
end;
  writeln((((('Sum of recipr. factors of ' + IntToStr(main_n)) + ' = ') + IntToStr(main_val)) + ' exactly ') + main_perfect);
end;
end;
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

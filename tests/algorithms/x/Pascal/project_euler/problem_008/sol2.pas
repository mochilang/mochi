{$mode objfpc}{$modeswitch nestedprocvars}
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
procedure json(x: int64);
begin
  writeln(x);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  N: string;
function solution(solution_n: string): int64; forward;
function solution(solution_n: string): int64;
var
  solution_max_product: int64;
  solution_i: int64;
  solution_product: int64;
  solution_j: int64;
begin
  solution_max_product := 0;
  solution_i := 0;
  while solution_i <= (Length(solution_n) - 13) do begin
  solution_product := 1;
  solution_j := 0;
  while solution_j < 13 do begin
  solution_product := solution_product * StrToInt(solution_n[solution_i + solution_j+1]);
  solution_j := solution_j + 1;
end;
  if solution_product > solution_max_product then begin
  solution_max_product := solution_product;
end;
  solution_i := solution_i + 1;
end;
  exit(solution_max_product);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  N := (((((((((((((((((('73167176531330624919225119674426574742355349194934' + '96983520312774506326239578318016984801869478851843') + '85861560789112949495459501737958331952853208805511') + '12540698747158523863050715693290963295227443043557') + '66896648950445244523161731856403098711121722383113') + '62229893423380308135336276614282806444486645238749') + '30358907296290491560440772390713810515859307960866') + '70172427121883998797908792274921901699720888093776') + '65727333001053367881220235421809751254540594752243') + '52584907711670556013604839586446706324415722155397') + '53697817977846174064955149290862569321978468622482') + '83972241375657056057490261407972968652414535100474') + '82166370484403199890008895243450658541227588666881') + '16427171479924442928230863465674813919123162824586') + '17866458359124566529476545682848912883142607690042') + '24219022671055626321111109370544217506941658960408') + '07198403850962455444362981230987879927244284909188') + '84580156166097919133875499200524063689912560717606') + '05886116467109405077541002256983155200055935729725') + '71636269561882670428252483600823257530420752963450';
  writeln(solution('13978431290823798458352374'));
  writeln(solution('13978431295823798458352374'));
  writeln(solution('1397843129582379841238352374'));
  writeln(solution(N));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

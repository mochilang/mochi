{$mode objfpc}
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
function list_int_to_str(xs: array of integer): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + IntToStr(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
type MDRResult = record
  mp: integer;
  mdr: integer;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  pad_out: string;
  mult_m: int64;
  mult_x: int64;
  mult_b: int64;
  multDigitalRoot_m: int64;
  multDigitalRoot_mp: integer;
  multDigitalRoot_b: int64;
  main_base: integer;
  main_size: integer;
  main_nums: array of int64;
  main_i: integer;
  main_n: int64;
  main_r: MDRResult;
  main_list: array of IntArray;
  main_idx: integer;
  main_cnt: integer;
  main_b: int64;
  main_mdr: integer;
  main_j: integer;
function makeMDRResult(mp: integer; mdr: integer): MDRResult; forward;
function pad(s: string; width: integer): string; forward;
function mult(n: int64; base: integer): int64; forward;
function multDigitalRoot(n: int64; base: integer): MDRResult; forward;
procedure main(); forward;
function makeMDRResult(mp: integer; mdr: integer): MDRResult;
begin
  Result.mp := mp;
  Result.mdr := mdr;
end;
function pad(s: string; width: integer): string;
begin
  pad_out := s;
  while Length(pad_out) < width do begin
  pad_out := ' ' + pad_out;
end;
  exit(pad_out);
end;
function mult(n: int64; base: integer): int64;
begin
  mult_m := 1;
  mult_x := n;
  mult_b := base;
  while mult_x > 0 do begin
  mult_m := mult_m * (mult_x mod mult_b);
  mult_x := mult_x div mult_b;
end;
  exit(mult_m);
end;
function multDigitalRoot(n: int64; base: integer): MDRResult;
begin
  multDigitalRoot_m := n;
  multDigitalRoot_mp := 0;
  multDigitalRoot_b := base;
  while multDigitalRoot_m >= multDigitalRoot_b do begin
  multDigitalRoot_m := mult(multDigitalRoot_m, base);
  multDigitalRoot_mp := multDigitalRoot_mp + 1;
end;
  exit(makeMDRResult(multDigitalRoot_mp, Trunc(multDigitalRoot_m)));
end;
procedure main();
begin
  main_base := 10;
  main_size := 5;
  writeln((((pad('Number', 20) + ' ') + pad('MDR', 3)) + ' ') + pad('MP', 3));
  main_nums := [123321, 7739, 893, 899998, 3778888999, 277777788888899];
  main_i := 0;
  while main_i < Length(main_nums) do begin
  main_n := main_nums[main_i];
  main_r := multDigitalRoot(main_n, main_base);
  writeln((((pad(IntToStr(main_n), 20) + ' ') + pad(IntToStr(main_r.mdr), 3)) + ' ') + pad(IntToStr(main_r.mp), 3));
  main_i := main_i + 1;
end;
  writeln('');
  main_list := [];
  main_idx := 0;
  while main_idx < main_base do begin
  main_list := concat(main_list, [[]]);
  main_idx := main_idx + 1;
end;
  main_cnt := main_size * main_base;
  main_n := 0;
  main_b := main_base;
  while main_cnt > 0 do begin
  main_r := multDigitalRoot(main_n, main_base);
  main_mdr := main_r.mdr;
  if Length(main_list[main_mdr]) < main_size then begin
  main_list[main_mdr] := concat(main_list[main_mdr], [Trunc(main_n)]);
  main_cnt := main_cnt - 1;
end;
  main_n := main_n + 1;
end;
  writeln('MDR: First');
  main_j := 0;
  while main_j < main_base do begin
  writeln((pad(IntToStr(main_j), 3) + ': ') + list_int_to_str(main_list[main_j]));
  main_j := main_j + 1;
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

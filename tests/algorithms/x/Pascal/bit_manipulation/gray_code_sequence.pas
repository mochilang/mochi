{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
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
function list_list_int_to_str(xs: array of IntArray): string;
var i: integer;
begin
  Result := '[';
  for i := 0 to High(xs) do begin
    Result := Result + list_int_to_str(xs[i]);
    if i < High(xs) then Result := Result + ' ';
  end;
  Result := Result + ']';
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  seq2: IntArray;
  seq1: IntArray;
  seq3: IntArray;
  exp: integer;
  bit_count: integer;
function pow2(exp: integer): integer; forward;
function gray_code(bit_count: integer): IntArray; forward;
function pow2(exp: integer): integer;
var
  pow2_result_: integer;
  pow2_i: integer;
begin
  pow2_result_ := 1;
  pow2_i := 0;
  while pow2_i < exp do begin
  pow2_result_ := pow2_result_ * 2;
  pow2_i := pow2_i + 1;
end;
  exit(pow2_result_);
end;
function gray_code(bit_count: integer): IntArray;
var
  gray_code_prev: array of integer;
  gray_code_add_val: integer;
  gray_code_res: array of integer;
  gray_code_i: integer;
  gray_code_j: integer;
begin
  if bit_count = 0 then begin
  exit([0]);
end;
  gray_code_prev := gray_code(bit_count - 1);
  gray_code_add_val := pow2(bit_count - 1);
  gray_code_res := [];
  gray_code_i := 0;
  while gray_code_i < Length(gray_code_prev) do begin
  gray_code_res := concat(gray_code_res, IntArray([gray_code_prev[gray_code_i]]));
  gray_code_i := gray_code_i + 1;
end;
  gray_code_j := Length(gray_code_prev) - 1;
  while gray_code_j >= 0 do begin
  gray_code_res := concat(gray_code_res, IntArray([gray_code_prev[gray_code_j] + gray_code_add_val]));
  gray_code_j := gray_code_j - 1;
end;
  exit(gray_code_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seq2 := gray_code(2);
  writeln(list_int_to_str(seq2));
  seq1 := gray_code(1);
  writeln(list_int_to_str(seq1));
  seq3 := gray_code(3);
  writeln(list_int_to_str(seq3));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

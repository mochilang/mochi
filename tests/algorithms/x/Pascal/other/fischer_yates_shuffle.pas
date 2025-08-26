{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
type StrArray = array of string;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
function list_int_to_str(xs: array of int64): string;
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
  seed: int64;
  integers: array of int64;
  strings: array of string;
function rand(): int64; forward;
function randint(randint_a: int64; randint_b: int64): int64; forward;
function fisher_yates_shuffle_int(fisher_yates_shuffle_int_data: IntArray): IntArray; forward;
function fisher_yates_shuffle_str(fisher_yates_shuffle_str_data: StrArray): StrArray; forward;
function rand(): int64;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483648;
  exit(_floordiv(seed, 65536));
end;
function randint(randint_a: int64; randint_b: int64): int64;
var
  randint_r: int64;
begin
  randint_r := rand();
  exit(randint_a + (randint_r mod ((randint_b - randint_a) + 1)));
end;
function fisher_yates_shuffle_int(fisher_yates_shuffle_int_data: IntArray): IntArray;
var
  fisher_yates_shuffle_int_res: array of int64;
  fisher_yates_shuffle_int_i: int64;
  fisher_yates_shuffle_int_a: int64;
  fisher_yates_shuffle_int_b: int64;
  fisher_yates_shuffle_int_temp: int64;
begin
  fisher_yates_shuffle_int_res := fisher_yates_shuffle_int_data;
  fisher_yates_shuffle_int_i := 0;
  while fisher_yates_shuffle_int_i < Length(fisher_yates_shuffle_int_res) do begin
  fisher_yates_shuffle_int_a := randint(0, Length(fisher_yates_shuffle_int_res) - 1);
  fisher_yates_shuffle_int_b := randint(0, Length(fisher_yates_shuffle_int_res) - 1);
  fisher_yates_shuffle_int_temp := fisher_yates_shuffle_int_res[fisher_yates_shuffle_int_a];
  fisher_yates_shuffle_int_res[fisher_yates_shuffle_int_a] := fisher_yates_shuffle_int_res[fisher_yates_shuffle_int_b];
  fisher_yates_shuffle_int_res[fisher_yates_shuffle_int_b] := fisher_yates_shuffle_int_temp;
  fisher_yates_shuffle_int_i := fisher_yates_shuffle_int_i + 1;
end;
  exit(fisher_yates_shuffle_int_res);
end;
function fisher_yates_shuffle_str(fisher_yates_shuffle_str_data: StrArray): StrArray;
var
  fisher_yates_shuffle_str_res: array of string;
  fisher_yates_shuffle_str_i: int64;
  fisher_yates_shuffle_str_a: int64;
  fisher_yates_shuffle_str_b: int64;
  fisher_yates_shuffle_str_temp: string;
begin
  fisher_yates_shuffle_str_res := fisher_yates_shuffle_str_data;
  fisher_yates_shuffle_str_i := 0;
  while fisher_yates_shuffle_str_i < Length(fisher_yates_shuffle_str_res) do begin
  fisher_yates_shuffle_str_a := randint(0, Length(fisher_yates_shuffle_str_res) - 1);
  fisher_yates_shuffle_str_b := randint(0, Length(fisher_yates_shuffle_str_res) - 1);
  fisher_yates_shuffle_str_temp := fisher_yates_shuffle_str_res[fisher_yates_shuffle_str_a];
  fisher_yates_shuffle_str_res[fisher_yates_shuffle_str_a] := fisher_yates_shuffle_str_res[fisher_yates_shuffle_str_b];
  fisher_yates_shuffle_str_res[fisher_yates_shuffle_str_b] := fisher_yates_shuffle_str_temp;
  fisher_yates_shuffle_str_i := fisher_yates_shuffle_str_i + 1;
end;
  exit(fisher_yates_shuffle_str_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  seed := 1;
  integers := [0, 1, 2, 3, 4, 5, 6, 7];
  strings := ['python', 'says', 'hello', '!'];
  writeln('Fisher-Yates Shuffle:');
  writeln((('List ' + list_int_to_str(integers)) + ' ') + list_to_str(strings));
  writeln((('FY Shuffle ' + list_int_to_str(fisher_yates_shuffle_int(integers))) + ' ') + list_to_str(fisher_yates_shuffle_str(strings)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
  writeln('');
end.

{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
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
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
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
  seed: integer;
  integers: array of integer;
  strings: array of string;
  data: StrArray;
  a: integer;
  b: integer;
function rand(): integer; forward;
function randint(a: integer; b: integer): integer; forward;
function fisher_yates_shuffle_int(data: IntArray): IntArray; forward;
function fisher_yates_shuffle_str(data: StrArray): StrArray; forward;
function rand(): integer;
begin
  seed := ((seed * 1103515245) + 12345) mod 2147483648;
  exit(seed div 65536);
end;
function randint(a: integer; b: integer): integer;
var
  randint_r: integer;
begin
  randint_r := rand();
  exit(a + (randint_r mod ((b - a) + 1)));
end;
function fisher_yates_shuffle_int(data: IntArray): IntArray;
var
  fisher_yates_shuffle_int_res: array of integer;
  fisher_yates_shuffle_int_i: integer;
  fisher_yates_shuffle_int_a: integer;
  fisher_yates_shuffle_int_b: integer;
  fisher_yates_shuffle_int_temp: integer;
begin
  fisher_yates_shuffle_int_res := data;
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
function fisher_yates_shuffle_str(data: StrArray): StrArray;
var
  fisher_yates_shuffle_str_res: array of string;
  fisher_yates_shuffle_str_i: integer;
  fisher_yates_shuffle_str_a: integer;
  fisher_yates_shuffle_str_b: integer;
  fisher_yates_shuffle_str_temp: string;
begin
  fisher_yates_shuffle_str_res := data;
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
end.

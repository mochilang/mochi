{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
  contains_key_int_k_idx: integer;
  data: string;
  bits: string;
  key: string;
  n: integer;
  m: specialize TFPGMap<string, integer>;
function Map1(): specialize TFPGMap<string, integer>; forward;
function to_binary(n: integer): string; forward;
function contains_key_int(m: specialize TFPGMap<string, integer>; key: string): boolean; forward;
function lzw_compress(bits: string): string; forward;
function Map1(): specialize TFPGMap<string, integer>;
begin
  Result := specialize TFPGMap<string, integer>.Create();
  Result.AddOrSetData('0', Variant(0));
  Result.AddOrSetData('1', Variant(1));
end;
function to_binary(n: integer): string;
var
  to_binary_num: integer;
  to_binary_res: string;
  to_binary_bit: integer;
begin
  if n = 0 then begin
  exit('0');
end;
  to_binary_num := n;
  to_binary_res := '';
  while to_binary_num > 0 do begin
  to_binary_bit := to_binary_num mod 2;
  to_binary_res := IntToStr(to_binary_bit) + to_binary_res;
  to_binary_num := to_binary_num div 2;
end;
  exit(to_binary_res);
end;
function contains_key_int(m: specialize TFPGMap<string, integer>; key: string): boolean;
var
  contains_key_int_k: string;
begin
  for contains_key_int_k_idx := 0 to (m.Count - 1) do begin
  contains_key_int_k := m.Keys[contains_key_int_k_idx];
  if contains_key_int_k = key then begin
  exit(true);
end;
end;
  exit(false);
end;
function lzw_compress(bits: string): string;
var
  lzw_compress_dict: specialize TFPGMap<string, integer>;
  lzw_compress_current: string;
  lzw_compress_result_: string;
  lzw_compress_index: integer;
  lzw_compress_i: integer;
  lzw_compress_ch: string;
  lzw_compress_candidate: string;
begin
  lzw_compress_dict := Map1();
  lzw_compress_current := '';
  lzw_compress_result_ := '';
  lzw_compress_index := 2;
  lzw_compress_i := 0;
  while lzw_compress_i < Length(bits) do begin
  lzw_compress_ch := bits[lzw_compress_i+1];
  lzw_compress_candidate := lzw_compress_current + lzw_compress_ch;
  if contains_key_int(lzw_compress_dict, lzw_compress_candidate) then begin
  lzw_compress_current := lzw_compress_candidate;
end else begin
  lzw_compress_result_ := lzw_compress_result_ + to_binary(lzw_compress_dict[lzw_compress_current]);
  lzw_compress_dict[lzw_compress_candidate] := lzw_compress_index;
  lzw_compress_index := lzw_compress_index + 1;
  lzw_compress_current := lzw_compress_ch;
end;
  lzw_compress_i := lzw_compress_i + 1;
end;
  if lzw_compress_current <> '' then begin
  lzw_compress_result_ := lzw_compress_result_ + to_binary(lzw_compress_dict[lzw_compress_current]);
end;
  exit(lzw_compress_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  data := '01001100100111';
  writeln(lzw_compress(data));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

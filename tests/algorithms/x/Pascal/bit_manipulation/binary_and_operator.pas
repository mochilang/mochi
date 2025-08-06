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
function to_binary(n: integer): string; forward;
function zfill(s: string; width: integer): string; forward;
function binary_and(a: integer; b: integer): string; forward;
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
function zfill(s: string; width: integer): string;
var
  zfill_res: string;
  zfill_pad: integer;
begin
  zfill_res := s;
  zfill_pad := width - Length(s);
  while zfill_pad > 0 do begin
  zfill_res := '0' + zfill_res;
  zfill_pad := zfill_pad - 1;
end;
  exit(zfill_res);
end;
function binary_and(a: integer; b: integer): string;
var
  binary_and_a_bin: string;
  binary_and_b_bin: string;
  binary_and_max_len: integer;
  binary_and_a_pad: string;
  binary_and_b_pad: string;
  binary_and_i: integer;
  binary_and_res: string;
begin
  if (a < 0) or (b < 0) then begin
  panic('the value of both inputs must be positive');
end;
  binary_and_a_bin := to_binary(a);
  binary_and_b_bin := to_binary(b);
  binary_and_max_len := Length(binary_and_a_bin);
  if Length(binary_and_b_bin) > binary_and_max_len then begin
  binary_and_max_len := Length(binary_and_b_bin);
end;
  binary_and_a_pad := zfill(binary_and_a_bin, binary_and_max_len);
  binary_and_b_pad := zfill(binary_and_b_bin, binary_and_max_len);
  binary_and_i := 0;
  binary_and_res := '';
  while binary_and_i < binary_and_max_len do begin
  if (binary_and_a_pad[binary_and_i+1] = '1') and (binary_and_b_pad[binary_and_i+1] = '1') then begin
  binary_and_res := binary_and_res + '1';
end else begin
  binary_and_res := binary_and_res + '0';
end;
  binary_and_i := binary_and_i + 1;
end;
  exit('0b' + binary_and_res);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(binary_and(25, 32));
  writeln(binary_and(37, 50));
  writeln(binary_and(21, 30));
  writeln(binary_and(58, 73));
  writeln(binary_and(0, 255));
  writeln(binary_and(256, 256));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

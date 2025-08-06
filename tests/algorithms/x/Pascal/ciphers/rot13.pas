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
  uppercase: string;
  lowercase: string;
  s: string;
  n: integer;
  c: string;
function index_of(s: string; c: string): integer; forward;
function dencrypt(s: string; n: integer): string; forward;
procedure main(); forward;
function index_of(s: string; c: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(s) do begin
  if copy(s, index_of_i+1, (index_of_i + 1 - (index_of_i))) = c then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function dencrypt(s: string; n: integer): string;
var
  dencrypt_out: string;
  dencrypt_i: integer;
  dencrypt_ch: string;
  dencrypt_idx_u: integer;
  dencrypt_new_idx: integer;
  dencrypt_idx_l: integer;
begin
  dencrypt_out := '';
  dencrypt_i := 0;
  while dencrypt_i < Length(s) do begin
  dencrypt_ch := copy(s, dencrypt_i+1, (dencrypt_i + 1 - (dencrypt_i)));
  dencrypt_idx_u := index_of(uppercase, dencrypt_ch);
  if dencrypt_idx_u >= 0 then begin
  dencrypt_new_idx := (dencrypt_idx_u + n) mod 26;
  dencrypt_out := dencrypt_out + copy(uppercase, dencrypt_new_idx+1, (dencrypt_new_idx + 1 - (dencrypt_new_idx)));
end else begin
  dencrypt_idx_l := index_of(lowercase, dencrypt_ch);
  if dencrypt_idx_l >= 0 then begin
  dencrypt_new_idx := (dencrypt_idx_l + n) mod 26;
  dencrypt_out := dencrypt_out + copy(lowercase, dencrypt_new_idx+1, (dencrypt_new_idx + 1 - (dencrypt_new_idx)));
end else begin
  dencrypt_out := dencrypt_out + dencrypt_ch;
end;
end;
  dencrypt_i := dencrypt_i + 1;
end;
  exit(dencrypt_out);
end;
procedure main();
var
  main_msg: string;
  main_s: string;
begin
  main_msg := 'My secret bank account number is 173-52946 so don''t tell anyone!!';
  main_s := dencrypt(main_msg, 13);
  writeln(main_s);
  writeln(LowerCase(BoolToStr(dencrypt(main_s, 13) = main_msg, true)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  uppercase := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  lowercase := 'abcdefghijklmnopqrstuvwxyz';
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

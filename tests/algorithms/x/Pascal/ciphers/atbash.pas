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
  c: string;
  sequence: string;
  s: string;
function index_of(s: string; c: string): integer; forward;
function atbash(sequence: string): string; forward;
function index_of(s: string; c: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(s) do begin
  if s[index_of_i+1] = c then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(-1);
end;
function atbash(sequence: string): string;
var
  atbash_lower: string;
  atbash_upper: string;
  atbash_lower_rev: string;
  atbash_upper_rev: string;
  atbash_result_: string;
  atbash_i: integer;
  atbash_ch: string;
  atbash_idx: integer;
  atbash_idx2: integer;
begin
  atbash_lower := 'abcdefghijklmnopqrstuvwxyz';
  atbash_upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  atbash_lower_rev := 'zyxwvutsrqponmlkjihgfedcba';
  atbash_upper_rev := 'ZYXWVUTSRQPONMLKJIHGFEDCBA';
  atbash_result_ := '';
  atbash_i := 0;
  while atbash_i < Length(sequence) do begin
  atbash_ch := sequence[atbash_i+1];
  atbash_idx := index_of(atbash_lower, atbash_ch);
  if atbash_idx <> -1 then begin
  atbash_result_ := atbash_result_ + atbash_lower_rev[atbash_idx+1];
end else begin
  atbash_idx2 := index_of(atbash_upper, atbash_ch);
  if atbash_idx2 <> -1 then begin
  atbash_result_ := atbash_result_ + atbash_upper_rev[atbash_idx2+1];
end else begin
  atbash_result_ := atbash_result_ + atbash_ch;
end;
end;
  atbash_i := atbash_i + 1;
end;
  exit(atbash_result_);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(atbash('ABCDEFGH'));
  writeln(atbash('123GGjj'));
  writeln(atbash('testStringtest'));
  writeln(atbash('with space'));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

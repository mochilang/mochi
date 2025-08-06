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
  LETTERS: string;
  ch: string;
  s: string;
  message: string;
function index_of(s: string; ch: string): integer; forward;
procedure decrypt(message: string); forward;
function index_of(s: string; ch: string): integer;
var
  index_of_i: integer;
begin
  index_of_i := 0;
  while index_of_i < Length(s) do begin
  if copy(s, index_of_i+1, (index_of_i + 1 - (index_of_i))) = ch then begin
  exit(index_of_i);
end;
  index_of_i := index_of_i + 1;
end;
  exit(0 - 1);
end;
procedure decrypt(message: string);
var
  decrypt_key: integer;
  decrypt_translated: string;
  decrypt_i: integer;
  decrypt_symbol: string;
  decrypt_idx: integer;
  decrypt_num: integer;
begin
  for decrypt_key := 0 to (Length(LETTERS) - 1) do begin
  decrypt_translated := '';
  for decrypt_i := 0 to (Length(message) - 1) do begin
  decrypt_symbol := copy(message, decrypt_i+1, (decrypt_i + 1 - (decrypt_i)));
  decrypt_idx := index_of(LETTERS, decrypt_symbol);
  if decrypt_idx <> (0 - 1) then begin
  decrypt_num := decrypt_idx - decrypt_key;
  if decrypt_num < 0 then begin
  decrypt_num := decrypt_num + Length(LETTERS);
end;
  decrypt_translated := decrypt_translated + copy(LETTERS, decrypt_num+1, (decrypt_num + 1 - (decrypt_num)));
end else begin
  decrypt_translated := decrypt_translated + decrypt_symbol;
end;
end;
  writeln((('Decryption using Key #' + IntToStr(decrypt_key)) + ': ') + decrypt_translated);
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  LETTERS := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  decrypt('TMDETUX PMDVU');
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

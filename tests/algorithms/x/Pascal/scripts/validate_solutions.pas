{$mode objfpc}
program Main;
uses SysUtils, Unix;
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
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
function _sha256(bs: IntArray): IntArray;
var tmp, outFile, hex: string; f: file; t: Text; i: integer; res: IntArray;
begin
  tmp := GetTempFileName('', 'mochi_sha256');
  Assign(f, tmp);
  Rewrite(f,1);
  for i := 0 to Length(bs)-1 do
    BlockWrite(f, bs[i],1);
  Close(f);
  outFile := tmp + '.hash';
  fpSystem(PChar(AnsiString('sha256sum ' + tmp + ' > ' + outFile)));
  Assign(t, outFile);
  Reset(t);
  ReadLn(t, hex);
  Close(t);
  DeleteFile(tmp);
  DeleteFile(outFile);
  hex := Trim(hex);
  SetLength(res, 32);
  for i := 0 to 31 do
    res[i] := StrToInt('$'+Copy(hex, i*2+1,2));
  _sha256 := res;
end;
function _sha256_str(s: string): IntArray;
var i: integer; bs: IntArray;
begin
  SetLength(bs, Length(s));
  for i := 1 to Length(s) do
    bs[i-1] := Ord(s[i]);
  _sha256_str := _sha256(bs);
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  HEX: string;
  expected: string;
  answer: string;
  computed: string;
  bs: IntArray;
  b: integer;
  s: string;
function byte_to_hex(b: integer): string; forward;
function bytes_to_hex(bs: IntArray): string; forward;
function sha256_hex(s: string): string; forward;
function solution_001(): string; forward;
function byte_to_hex(b: integer): string;
var
  byte_to_hex_hi: integer;
  byte_to_hex_lo: integer;
begin
  byte_to_hex_hi := b div 16;
  byte_to_hex_lo := b mod 16;
  exit(HEX[byte_to_hex_hi+1] + HEX[byte_to_hex_lo+1]);
end;
function bytes_to_hex(bs: IntArray): string;
var
  bytes_to_hex_res: string;
  bytes_to_hex_i: integer;
begin
  bytes_to_hex_res := '';
  bytes_to_hex_i := 0;
  while bytes_to_hex_i < Length(bs) do begin
  bytes_to_hex_res := bytes_to_hex_res + byte_to_hex(bs[bytes_to_hex_i]);
  bytes_to_hex_i := bytes_to_hex_i + 1;
end;
  exit(bytes_to_hex_res);
end;
function sha256_hex(s: string): string;
begin
  exit(bytes_to_hex(_sha256_str(s)));
end;
function solution_001(): string;
var
  solution_001_total: integer;
  solution_001_n: integer;
begin
  solution_001_total := 0;
  solution_001_n := 0;
  while solution_001_n < 1000 do begin
  if ((solution_001_n mod 3) = 0) or ((solution_001_n mod 5) = 0) then begin
  solution_001_total := solution_001_total + solution_001_n;
end;
  solution_001_n := solution_001_n + 1;
end;
  exit(IntToStr(solution_001_total));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  HEX := '0123456789abcdef';
  expected := sha256_hex('233168');
  answer := solution_001();
  computed := sha256_hex(answer);
  if computed = expected then begin
  writeln('Problem 001 passed');
end else begin
  writeln((('Problem 001 failed: ' + computed) + ' != ') + expected);
end;
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

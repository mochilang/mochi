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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  ch: string;
  a: IntArray;
function indexOf(s: string; ch: string): integer; forward;
function set58(addr: string): IntArray; forward;
function doubleSHA256(bs: IntArray): IntArray; forward;
function computeChecksum(a: IntArray): IntArray; forward;
function validA58(addr: string): boolean; forward;
function indexOf(s: string; ch: string): integer;
var
  indexOf_i: integer;
begin
  indexOf_i := 0;
  while indexOf_i < Length(s) do begin
  if copy(s, indexOf_i+1, (indexOf_i + 1 - (indexOf_i))) = ch then begin
  exit(indexOf_i);
end;
  indexOf_i := indexOf_i + 1;
end;
  exit(-1);
end;
function set58(addr: string): IntArray;
var
  set58_tmpl: string;
  set58_a: array of integer;
  set58_i: integer;
  set58_idx: integer;
  set58_c: integer;
  set58_j: integer;
begin
  set58_tmpl := '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz';
  set58_a := [];
  set58_i := 0;
  while set58_i < 25 do begin
  set58_a := concat(set58_a, [0]);
  set58_i := set58_i + 1;
end;
  set58_idx := 0;
  while set58_idx < Length(addr) do begin
  ch := copy(addr, set58_idx+1, (set58_idx + 1 - (set58_idx)));
  set58_c := indexOf(set58_tmpl, ch);
  if set58_c < 0 then begin
  exit([]);
end;
  set58_j := 24;
  while set58_j >= 0 do begin
  set58_c := set58_c + (58 * set58_a[set58_j]);
  set58_a[set58_j] := set58_c mod 256;
  set58_c := Trunc(set58_c div 256);
  set58_j := set58_j - 1;
end;
  if set58_c > 0 then begin
  exit([]);
end;
  set58_idx := set58_idx + 1;
end;
  exit(set58_a);
end;
function doubleSHA256(bs: IntArray): IntArray;
var
  doubleSHA256_first: array of integer;
begin
  doubleSHA256_first := _sha256(bs);
  exit(_sha256(doubleSHA256_first));
end;
function computeChecksum(a: IntArray): IntArray;
var
  computeChecksum_hash: IntArray;
begin
  computeChecksum_hash := doubleSHA256(copy(a, 0, (21 - (0))));
  exit(copy(computeChecksum_hash, 0, (4 - (0))));
end;
function validA58(addr: string): boolean;
var
  validA58_sum: IntArray;
  validA58_i: integer;
begin
  a := set58(addr);
  if Length(a) <> 25 then begin
  exit(false);
end;
  if a[0] <> 0 then begin
  exit(false);
end;
  validA58_sum := computeChecksum(a);
  validA58_i := 0;
  while validA58_i < 4 do begin
  if a[21 + validA58_i] <> validA58_sum[validA58_i] then begin
  exit(false);
end;
  validA58_i := validA58_i + 1;
end;
  exit(true);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(LowerCase(BoolToStr(validA58('1AGNa15ZQXAZUgFiqJ3i7Z2DPU2J6hW62i'), true)));
  writeln(LowerCase(BoolToStr(validA58('17NdbrSGoUotzeGCcMMCqnFkEvLymoou9j'), true)));
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

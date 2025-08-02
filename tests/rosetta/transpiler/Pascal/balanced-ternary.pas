{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  b: array of integer;
  i: integer;
  a: integer;
  a_idx: integer;
function Map2(btString_b: IntArray): specialize TFPGMap<string, Variant>; forward;
function Map1(): specialize TFPGMap<string, Variant>; forward;
function trimLeftZeros(s: string): string; forward;
function btString(s: string): specialize TFPGMap<string, Variant>; forward;
function btToString(b: IntArray): string; forward;
function btInt(i: integer): IntArray; forward;
function btToInt(b: IntArray): integer; forward;
function btNeg(b: IntArray): IntArray; forward;
function btAdd(a: IntArray; b: IntArray): IntArray; forward;
function btMul(a: IntArray; b: IntArray): IntArray; forward;
function padLeft(s: string; w: integer): string; forward;
procedure show(label_: string; b: IntArray); forward;
procedure main(); forward;
function Map2(btString_b: IntArray): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('bt', Variant(btString_b));
  Result.AddOrSetData('ok', Variant(true));
end;
function Map1(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('bt', Variant([]));
  Result.AddOrSetData('ok', Variant(false));
end;
function trimLeftZeros(s: string): string;
var
  trimLeftZeros_i: integer;
begin
  trimLeftZeros_i := 0;
  while (trimLeftZeros_i < Length(s)) and (copy(s, trimLeftZeros_i+1, (trimLeftZeros_i + 1 - (trimLeftZeros_i))) = '0') do begin
  trimLeftZeros_i := trimLeftZeros_i + 1;
end;
  exit(copy(s, trimLeftZeros_i+1, (Length(s) - (trimLeftZeros_i))));
end;
function btString(s: string): specialize TFPGMap<string, Variant>;
var
  btString_b: array of integer;
  btString_i: integer;
  btString_ch: string;
begin
  s := trimLeftZeros(s);
  btString_b := [];
  btString_i := Length(s) - 1;
  while btString_i >= 0 do begin
  btString_ch := copy(s, btString_i+1, (btString_i + 1 - (btString_i)));
  if btString_ch = '+' then begin
  btString_b := concat(btString_b, [1]);
end else begin
  if btString_ch = '0' then begin
  btString_b := concat(btString_b, [0]);
end else begin
  if btString_ch = '-' then begin
  btString_b := concat(btString_b, [0 - 1]);
end else begin
  exit(Map1());
end;
end;
end;
  btString_i := btString_i - 1;
end;
  exit(Map2(btString_b));
end;
function btToString(b: IntArray): string;
var
  btToString_r: string;
  btToString_i: integer;
  btToString_d: integer;
begin
  if Length(b) = 0 then begin
  exit('0');
end;
  btToString_r := '';
  btToString_i := Length(b) - 1;
  while btToString_i >= 0 do begin
  btToString_d := b[btToString_i];
  if btToString_d = (0 - 1) then begin
  btToString_r := btToString_r + '-';
end else begin
  if btToString_d = 0 then begin
  btToString_r := btToString_r + '0';
end else begin
  btToString_r := btToString_r + '+';
end;
end;
  btToString_i := btToString_i - 1;
end;
  exit(btToString_r);
end;
function btInt(i: integer): IntArray;
var
  btInt_n: integer;
  btInt_m: integer;
begin
  if i = 0 then begin
  exit([]);
end;
  btInt_n := i;
  b := [];
  while btInt_n <> 0 do begin
  btInt_m := btInt_n mod 3;
  btInt_n := Trunc(btInt_n div 3);
  if btInt_m = 2 then begin
  btInt_m := 0 - 1;
  btInt_n := btInt_n + 1;
end else begin
  if btInt_m = (0 - 2) then begin
  btInt_m := 1;
  btInt_n := btInt_n - 1;
end;
end;
  b := concat(b, [btInt_m]);
end;
  exit(b);
end;
function btToInt(b: IntArray): integer;
var
  btToInt_r: integer;
  btToInt_pt: integer;
begin
  btToInt_r := 0;
  btToInt_pt := 1;
  i := 0;
  while i < Length(b) do begin
  btToInt_r := btToInt_r + (b[i] * btToInt_pt);
  btToInt_pt := btToInt_pt * 3;
  i := i + 1;
end;
  exit(btToInt_r);
end;
function btNeg(b: IntArray): IntArray;
var
  btNeg_r: array of integer;
begin
  btNeg_r := [];
  i := 0;
  while i < Length(b) do begin
  btNeg_r := concat(btNeg_r, [-b[i]]);
  i := i + 1;
end;
  exit(btNeg_r);
end;
function btAdd(a: IntArray; b: IntArray): IntArray;
begin
  exit(btInt(btToInt(a) + btToInt(b)));
end;
function btMul(a: IntArray; b: IntArray): IntArray;
begin
  exit(btInt(btToInt(a) * btToInt(b)));
end;
function padLeft(s: string; w: integer): string;
var
  padLeft_r: string;
begin
  padLeft_r := s;
  while Length(padLeft_r) < w do begin
  padLeft_r := ' ' + padLeft_r;
end;
  exit(padLeft_r);
end;
procedure show(label_: string; b: IntArray);
var
  show_l: string;
  show_bs: string;
  show_is: string;
begin
  show_l := padLeft(label_, 7);
  show_bs := padLeft(btToString(b), 12);
  show_is := padLeft(IntToStr(btToInt(b)), 7);
  writeln((((show_l + ' ') + show_bs) + ' ') + show_is);
end;
procedure main();
var
  main_ares: specialize TFPGMap<string, Variant>;
  main_cres: specialize TFPGMap<string, Variant>;
  main_c: integer;
  main_c_idx: integer;
begin
  main_ares := btString('+-0++0+');
  a_idx := main_ares.IndexOf('bt');
  if a_idx <> -1 then begin
  a := main_ares.Data[a_idx];
end else begin
  a := 0;
end;
  b := btInt(-436);
  main_cres := btString('+-++-');
  main_c_idx := main_cres.IndexOf('bt');
  if main_c_idx <> -1 then begin
  main_c := main_cres.Data[main_c_idx];
end else begin
  main_c := 0;
end;
  show('a:', a);
  show('b:', b);
  show('c:', main_c);
  show('a(b-c):', btMul(a, btAdd(b, btNeg(main_c))));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  Sleep(1);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

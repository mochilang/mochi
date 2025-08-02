{$mode objfpc}
program Main;
uses SysUtils;
type RealArray = array of real;
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
function indexOf(s: string; ch: string): integer; forward;
function fmt3(x: real): string; forward;
function pad(s: string; width: integer): string; forward;
function smaSeries(xs: RealArray; period: integer): RealArray; forward;
procedure main(); forward;
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
function fmt3(x: real): string;
var
  fmt3_y: real;
  fmt3_s: string;
  fmt3_dot: integer;
  fmt3_decs: integer;
begin
  fmt3_y := Double(Trunc((x * 1000) + 0.5)) / 1000;
  fmt3_s := FloatToStr(fmt3_y);
  fmt3_dot := indexOf(fmt3_s, '.');
  if fmt3_dot = (0 - 1) then begin
  fmt3_s := fmt3_s + '.000';
end else begin
  fmt3_decs := (Length(fmt3_s) - fmt3_dot) - 1;
  if fmt3_decs > 3 then begin
  fmt3_s := copy(fmt3_s, 0+1, (fmt3_dot + 4 - (0)));
end else begin
  while fmt3_decs < 3 do begin
  fmt3_s := fmt3_s + '0';
  fmt3_decs := fmt3_decs + 1;
end;
end;
end;
  exit(fmt3_s);
end;
function pad(s: string; width: integer): string;
var
  pad_out: string;
begin
  pad_out := s;
  while Length(pad_out) < width do begin
  pad_out := ' ' + pad_out;
end;
  exit(pad_out);
end;
function smaSeries(xs: RealArray; period: integer): RealArray;
var
  smaSeries_res: array of real;
  smaSeries_sum: real;
  smaSeries_i: integer;
  smaSeries_denom: integer;
begin
  smaSeries_res := [];
  smaSeries_sum := 0;
  smaSeries_i := 0;
  while smaSeries_i < Length(xs) do begin
  smaSeries_sum := smaSeries_sum + xs[smaSeries_i];
  if smaSeries_i >= period then begin
  smaSeries_sum := smaSeries_sum - xs[smaSeries_i - period];
end;
  smaSeries_denom := smaSeries_i + 1;
  if smaSeries_denom > period then begin
  smaSeries_denom := period;
end;
  smaSeries_res := concat(smaSeries_res, [smaSeries_sum / Double(smaSeries_denom)]);
  smaSeries_i := smaSeries_i + 1;
end;
  exit(smaSeries_res);
end;
procedure main();
var
  main_xs: array of real;
  main_sma3: RealArray;
  main_sma5: RealArray;
  main_i: integer;
  main_line: string;
begin
  main_xs := [1, 2, 3, 4, 5, 5, 4, 3, 2, 1];
  main_sma3 := smaSeries(main_xs, 3);
  main_sma5 := smaSeries(main_xs, 5);
  writeln('x       sma3   sma5');
  main_i := 0;
  while main_i < Length(main_xs) do begin
  main_line := (((pad(fmt3(main_xs[main_i]), 5) + '  ') + pad(fmt3(main_sma3[main_i]), 5)) + '  ') + pad(fmt3(main_sma5[main_i]), 5);
  writeln(main_line);
  main_i := main_i + 1;
end;
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

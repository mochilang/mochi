{$mode objfpc}
program Main;
uses SysUtils;
type StrArray = array of string;
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
  PI: real;
function sinApprox(x: real): real; forward;
function cosApprox(x: real): real; forward;
function atanApprox(x: real): real; forward;
function atan2Approx(y: real; x: real): real; forward;
function digit(ch: string): integer; forward;
function parseTwo(s: string; idx: integer): integer; forward;
function parseSec(s: string): real; forward;
function pad(n: integer): string; forward;
function meanTime(times: StrArray): string; forward;
procedure main(); forward;
function sinApprox(x: real): real;
var
  sinApprox_term: real;
  sinApprox_sum: real;
  sinApprox_n: integer;
  sinApprox_denom: real;
begin
  sinApprox_term := x;
  sinApprox_sum := x;
  sinApprox_n := 1;
  while sinApprox_n <= 8 do begin
  sinApprox_denom := Double((2 * sinApprox_n) * ((2 * sinApprox_n) + 1));
  sinApprox_term := ((-sinApprox_term * x) * x) / sinApprox_denom;
  sinApprox_sum := sinApprox_sum + sinApprox_term;
  sinApprox_n := sinApprox_n + 1;
end;
  exit(sinApprox_sum);
end;
function cosApprox(x: real): real;
var
  cosApprox_term: real;
  cosApprox_sum: real;
  cosApprox_n: integer;
  cosApprox_denom: real;
begin
  cosApprox_term := 1;
  cosApprox_sum := 1;
  cosApprox_n := 1;
  while cosApprox_n <= 8 do begin
  cosApprox_denom := Double(((2 * cosApprox_n) - 1) * (2 * cosApprox_n));
  cosApprox_term := ((-cosApprox_term * x) * x) / cosApprox_denom;
  cosApprox_sum := cosApprox_sum + cosApprox_term;
  cosApprox_n := cosApprox_n + 1;
end;
  exit(cosApprox_sum);
end;
function atanApprox(x: real): real;
begin
  if x > 1 then begin
  exit((PI / 2) - (x / ((x * x) + 0.28)));
end;
  if x < -1 then begin
  exit((-PI / 2) - (x / ((x * x) + 0.28)));
end;
  exit(x / (1 + ((0.28 * x) * x)));
end;
function atan2Approx(y: real; x: real): real;
begin
  if x > 0 then begin
  exit(atanApprox(y / x));
end;
  if x < 0 then begin
  if y >= 0 then begin
  exit(atanApprox(y / x) + PI);
end;
  exit(atanApprox(y / x) - PI);
end;
  if y > 0 then begin
  exit(PI / 2);
end;
  if y < 0 then begin
  exit(-PI / 2);
end;
  exit(0);
end;
function digit(ch: string): integer;
var
  digit_digits: string;
  digit_i: integer;
begin
  digit_digits := '0123456789';
  digit_i := 0;
  while digit_i < Length(digit_digits) do begin
  if copy(digit_digits, digit_i+1, (digit_i + 1 - (digit_i))) = ch then begin
  exit(digit_i);
end;
  digit_i := digit_i + 1;
end;
  exit(0);
end;
function parseTwo(s: string; idx: integer): integer;
begin
  exit((digit(copy(s, idx+1, (idx + 1 - (idx)))) * 10) + digit(copy(s, idx + 1+1, (idx + 2 - (idx + 1)))));
end;
function parseSec(s: string): real;
var
  parseSec_h: integer;
  parseSec_m: integer;
  parseSec_sec: integer;
  parseSec_tmp: integer;
begin
  parseSec_h := parseTwo(s, 0);
  parseSec_m := parseTwo(s, 3);
  parseSec_sec := parseTwo(s, 6);
  parseSec_tmp := (((parseSec_h * 60) + parseSec_m) * 60) + parseSec_sec;
  exit(Double(parseSec_tmp));
end;
function pad(n: integer): string;
begin
  if n < 10 then begin
  exit('0' + IntToStr(n));
end;
  exit(IntToStr(n));
end;
function meanTime(times: StrArray): string;
var
  meanTime_ssum: real;
  meanTime_csum: real;
  meanTime_i: integer;
  meanTime_sec: real;
  meanTime_ang: real;
  meanTime_theta: real;
  meanTime_frac: real;
  meanTime_total: real;
  meanTime_si: integer;
  meanTime_h: integer;
  meanTime_m: integer;
  meanTime_s: integer;
begin
  meanTime_ssum := 0;
  meanTime_csum := 0;
  meanTime_i := 0;
  while meanTime_i < Length(times) do begin
  meanTime_sec := parseSec(times[meanTime_i]);
  meanTime_ang := ((meanTime_sec * 2) * PI) / 86400;
  meanTime_ssum := meanTime_ssum + sinApprox(meanTime_ang);
  meanTime_csum := meanTime_csum + cosApprox(meanTime_ang);
  meanTime_i := meanTime_i + 1;
end;
  meanTime_theta := atan2Approx(meanTime_ssum, meanTime_csum);
  meanTime_frac := meanTime_theta / (2 * PI);
  while meanTime_frac < 0 do begin
  meanTime_frac := meanTime_frac + 1;
end;
  meanTime_total := meanTime_frac * 86400;
  meanTime_si := Trunc(meanTime_total);
  meanTime_h := Trunc(meanTime_si div 3600);
  meanTime_m := Trunc((meanTime_si mod 3600) div 60);
  meanTime_s := Trunc(meanTime_si mod 60);
  exit((((pad(meanTime_h) + ':') + pad(meanTime_m)) + ':') + pad(meanTime_s));
end;
procedure main();
var
  main_inputs: array of string;
begin
  main_inputs := ['23:00:17', '23:40:20', '00:12:45', '00:17:19'];
  writeln(meanTime(main_inputs));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  PI := 3.141592653589793;
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

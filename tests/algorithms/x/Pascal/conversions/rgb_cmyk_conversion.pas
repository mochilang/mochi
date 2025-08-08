{$mode objfpc}
program Main;
uses SysUtils;
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
procedure show_list(xs: array of integer);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(' ');
  end;
  write(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  x: real;
  b_input: integer;
  g_input: integer;
  r_input: integer;
function round_int(x: real): integer; forward;
function rgb_to_cmyk(r_input: integer; g_input: integer; b_input: integer): IntArray; forward;
function round_int(x: real): integer;
begin
  exit(Trunc(x + 0.5));
end;
function rgb_to_cmyk(r_input: integer; g_input: integer; b_input: integer): IntArray;
var
  rgb_to_cmyk_r: real;
  rgb_to_cmyk_g: real;
  rgb_to_cmyk_b: real;
  rgb_to_cmyk_max_val: real;
  rgb_to_cmyk_k_float: real;
  rgb_to_cmyk_c_float: real;
  rgb_to_cmyk_m_float: real;
  rgb_to_cmyk_y_float: real;
  rgb_to_cmyk_k_percent: real;
  rgb_to_cmyk_c: integer;
  rgb_to_cmyk_m: integer;
  rgb_to_cmyk_y: integer;
  rgb_to_cmyk_k: integer;
begin
  if (((((r_input < 0) or (r_input >= 256)) or (g_input < 0)) or (g_input >= 256)) or (b_input < 0)) or (b_input >= 256) then begin
  panic('Expected int of the range 0..255');
end;
  rgb_to_cmyk_r := Double(r_input) / 255;
  rgb_to_cmyk_g := Double(g_input) / 255;
  rgb_to_cmyk_b := Double(b_input) / 255;
  rgb_to_cmyk_max_val := rgb_to_cmyk_r;
  if rgb_to_cmyk_g > rgb_to_cmyk_max_val then begin
  rgb_to_cmyk_max_val := rgb_to_cmyk_g;
end;
  if rgb_to_cmyk_b > rgb_to_cmyk_max_val then begin
  rgb_to_cmyk_max_val := rgb_to_cmyk_b;
end;
  rgb_to_cmyk_k_float := 1 - rgb_to_cmyk_max_val;
  if rgb_to_cmyk_k_float = 1 then begin
  exit([0, 0, 0, 100]);
end;
  rgb_to_cmyk_c_float := (100 * ((1 - rgb_to_cmyk_r) - rgb_to_cmyk_k_float)) / (1 - rgb_to_cmyk_k_float);
  rgb_to_cmyk_m_float := (100 * ((1 - rgb_to_cmyk_g) - rgb_to_cmyk_k_float)) / (1 - rgb_to_cmyk_k_float);
  rgb_to_cmyk_y_float := (100 * ((1 - rgb_to_cmyk_b) - rgb_to_cmyk_k_float)) / (1 - rgb_to_cmyk_k_float);
  rgb_to_cmyk_k_percent := 100 * rgb_to_cmyk_k_float;
  rgb_to_cmyk_c := round_int(rgb_to_cmyk_c_float);
  rgb_to_cmyk_m := round_int(rgb_to_cmyk_m_float);
  rgb_to_cmyk_y := round_int(rgb_to_cmyk_y_float);
  rgb_to_cmyk_k := round_int(rgb_to_cmyk_k_percent);
  exit([rgb_to_cmyk_c, rgb_to_cmyk_m, rgb_to_cmyk_y, rgb_to_cmyk_k]);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  show_list(rgb_to_cmyk(255, 255, 255));
  show_list(rgb_to_cmyk(128, 128, 128));
  show_list(rgb_to_cmyk(0, 0, 0));
  show_list(rgb_to_cmyk(255, 0, 0));
  show_list(rgb_to_cmyk(0, 255, 0));
  show_list(rgb_to_cmyk(0, 0, 255));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

{$mode objfpc}
program Main;
uses SysUtils;
type IntArray = array of integer;
type IntArrayArray = array of IntArray;
type IntArrayArrayArray = array of IntArrayArray;
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
  threshold: integer;
  h: integer;
  blue: integer;
  green: integer;
  red: integer;
  img: IntArrayArrayArray;
  w: integer;
function get_greyscale(blue: integer; green: integer; red: integer): integer; forward;
function zeros(h: integer; w: integer): IntArrayArray; forward;
function burkes_dither(img: IntArrayArrayArray; threshold: integer): IntArrayArray; forward;
procedure main(); forward;
function get_greyscale(blue: integer; green: integer; red: integer): integer;
var
  get_greyscale_b: real;
  get_greyscale_g: real;
  get_greyscale_r: real;
begin
  get_greyscale_b := Double(blue);
  get_greyscale_g := Double(green);
  get_greyscale_r := Double(red);
  exit(Trunc(((0.114 * get_greyscale_b) + (0.587 * get_greyscale_g)) + (0.299 * get_greyscale_r)));
end;
function zeros(h: integer; w: integer): IntArrayArray;
var
  zeros_table: array of IntArray;
  zeros_i: integer;
  zeros_row: array of integer;
  zeros_j: integer;
begin
  zeros_table := [];
  zeros_i := 0;
  while zeros_i < h do begin
  zeros_row := [];
  zeros_j := 0;
  while zeros_j < w do begin
  zeros_row := concat(zeros_row, IntArray([0]));
  zeros_j := zeros_j + 1;
end;
  zeros_table := concat(zeros_table, [zeros_row]);
  zeros_i := zeros_i + 1;
end;
  exit(zeros_table);
end;
function burkes_dither(img: IntArrayArrayArray; threshold: integer): IntArrayArray;
var
  burkes_dither_height: integer;
  burkes_dither_width: integer;
  burkes_dither_error_table: IntArrayArray;
  burkes_dither_output: array of IntArray;
  burkes_dither_y: integer;
  burkes_dither_row: array of integer;
  burkes_dither_x: integer;
  burkes_dither_px: array of integer;
  burkes_dither_grey: integer;
  burkes_dither_total: integer;
  burkes_dither_new_val: integer;
  burkes_dither_current_error: integer;
begin
  burkes_dither_height := Length(img);
  burkes_dither_width := Length(img[0]);
  burkes_dither_error_table := zeros(burkes_dither_height + 1, burkes_dither_width + 4);
  burkes_dither_output := [];
  burkes_dither_y := 0;
  while burkes_dither_y < burkes_dither_height do begin
  burkes_dither_row := [];
  burkes_dither_x := 0;
  while burkes_dither_x < burkes_dither_width do begin
  burkes_dither_px := img[burkes_dither_y][burkes_dither_x];
  burkes_dither_grey := get_greyscale(burkes_dither_px[0], burkes_dither_px[1], burkes_dither_px[2]);
  burkes_dither_total := burkes_dither_grey + burkes_dither_error_table[burkes_dither_y][burkes_dither_x + 2];
  burkes_dither_new_val := 0;
  burkes_dither_current_error := 0;
  if threshold > burkes_dither_total then begin
  burkes_dither_new_val := 0;
  burkes_dither_current_error := burkes_dither_total;
end else begin
  burkes_dither_new_val := 255;
  burkes_dither_current_error := burkes_dither_total - 255;
end;
  burkes_dither_row := concat(burkes_dither_row, IntArray([burkes_dither_new_val]));
  burkes_dither_error_table[burkes_dither_y][burkes_dither_x + 3] := burkes_dither_error_table[burkes_dither_y][burkes_dither_x + 3] + ((8 * burkes_dither_current_error) div 32);
  burkes_dither_error_table[burkes_dither_y][burkes_dither_x + 4] := burkes_dither_error_table[burkes_dither_y][burkes_dither_x + 4] + ((4 * burkes_dither_current_error) div 32);
  burkes_dither_error_table[burkes_dither_y + 1][burkes_dither_x + 2] := burkes_dither_error_table[burkes_dither_y + 1][burkes_dither_x + 2] + ((8 * burkes_dither_current_error) div 32);
  burkes_dither_error_table[burkes_dither_y + 1][burkes_dither_x + 3] := burkes_dither_error_table[burkes_dither_y + 1][burkes_dither_x + 3] + ((4 * burkes_dither_current_error) div 32);
  burkes_dither_error_table[burkes_dither_y + 1][burkes_dither_x + 4] := burkes_dither_error_table[burkes_dither_y + 1][burkes_dither_x + 4] + ((2 * burkes_dither_current_error) div 32);
  burkes_dither_error_table[burkes_dither_y + 1][burkes_dither_x + 1] := burkes_dither_error_table[burkes_dither_y + 1][burkes_dither_x + 1] + ((4 * burkes_dither_current_error) div 32);
  burkes_dither_error_table[burkes_dither_y + 1][burkes_dither_x] := burkes_dither_error_table[burkes_dither_y + 1][burkes_dither_x] + ((2 * burkes_dither_current_error) div 32);
  burkes_dither_x := burkes_dither_x + 1;
end;
  burkes_dither_output := concat(burkes_dither_output, [burkes_dither_row]);
  burkes_dither_y := burkes_dither_y + 1;
end;
  exit(burkes_dither_output);
end;
procedure main();
var
  main_img: array of IntArrayArray;
  main_result_: IntArrayArray;
  main_y: integer;
  main_line: string;
  main_x: integer;
begin
  main_img := [[[0, 0, 0], [64, 64, 64], [128, 128, 128], [192, 192, 192]], [[255, 255, 255], [200, 200, 200], [150, 150, 150], [100, 100, 100]], [[30, 144, 255], [255, 0, 0], [0, 255, 0], [0, 0, 255]], [[50, 100, 150], [80, 160, 240], [70, 140, 210], [60, 120, 180]]];
  main_result_ := burkes_dither(main_img, 128);
  main_y := 0;
  while main_y < Length(main_result_) do begin
  main_line := '';
  main_x := 0;
  while main_x < Length(main_result_[main_y]) do begin
  main_line := main_line + IntToStr(main_result_[main_y][main_x]);
  if main_x < (Length(main_result_[main_y]) - 1) then begin
  main_line := main_line + ' ';
end;
  main_x := main_x + 1;
end;
  writeln(main_line);
  main_y := main_y + 1;
end;
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

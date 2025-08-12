{$mode objfpc}{$modeswitch nestedprocvars}
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
  object_distance_from_lens: real;
  image_distance_from_lens: real;
  focal_length_of_lens_var: real;
function focal_length_of_lens(object_distance_from_lens: real; image_distance_from_lens: real): real; forward;
function object_distance(focal_length_of_lens_var: real; image_distance_from_lens: real): real; forward;
function image_distance(focal_length_of_lens_var: real; object_distance_from_lens: real): real; forward;
function focal_length_of_lens(object_distance_from_lens: real; image_distance_from_lens: real): real;
begin
  if (object_distance_from_lens = 0) or (image_distance_from_lens = 0) then begin
  panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
end;
  exit(1 / ((1 / image_distance_from_lens) - (1 / object_distance_from_lens)));
end;
function object_distance(focal_length_of_lens_var: real; image_distance_from_lens: real): real;
begin
  if (image_distance_from_lens = 0) or (focal_length_of_lens_var = 0) then begin
  panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
end;
  exit(1 / ((1 / image_distance_from_lens) - (1 / focal_length_of_lens_var)));
end;
function image_distance(focal_length_of_lens_var: real; object_distance_from_lens: real): real;
begin
  if (object_distance_from_lens = 0) or (focal_length_of_lens_var = 0) then begin
  panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
end;
  exit(1 / ((1 / object_distance_from_lens) + (1 / focal_length_of_lens_var)));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  writeln(FloatToStr(focal_length_of_lens(10, 4)));
  writeln(FloatToStr(focal_length_of_lens(2.7, 5.8)));
  writeln(FloatToStr(object_distance(10, 40)));
  writeln(FloatToStr(object_distance(6.2, 1.5)));
  writeln(FloatToStr(image_distance(50, 40)));
  writeln(FloatToStr(image_distance(5.3, 7.9)));
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

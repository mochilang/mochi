{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Math;
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
procedure error(msg: string);
begin
  panic(msg);
end;
function _to_float(x: integer): real;
begin
  _to_float := x;
end;
function to_float(x: integer): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real);
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
function min3(min3_a: integer; min3_b: integer; min3_c: integer): integer; forward;
function edit_distance(edit_distance_source: string; edit_distance_target: string): integer; forward;
procedure main(); forward;
function min3(min3_a: integer; min3_b: integer; min3_c: integer): integer;
var
  min3_m: integer;
begin
  min3_m := min3_a;
  if min3_b < min3_m then begin
  min3_m := min3_b;
end;
  if min3_c < min3_m then begin
  min3_m := min3_c;
end;
  exit(min3_m);
end;
function edit_distance(edit_distance_source: string; edit_distance_target: string): integer;
var
  edit_distance_last_source: string;
  edit_distance_last_target: string;
  edit_distance_delta: integer;
  edit_distance_delete_cost: integer;
  edit_distance_insert_cost: integer;
  edit_distance_replace_cost: integer;
begin
  if Length(edit_distance_source) = 0 then begin
  exit(Length(edit_distance_target));
end;
  if Length(edit_distance_target) = 0 then begin
  exit(Length(edit_distance_source));
end;
  edit_distance_last_source := copy(edit_distance_source, Length(edit_distance_source) - 1+1, (Length(edit_distance_source) - (Length(edit_distance_source) - 1)));
  edit_distance_last_target := copy(edit_distance_target, Length(edit_distance_target) - 1+1, (Length(edit_distance_target) - (Length(edit_distance_target) - 1)));
  if edit_distance_last_source = edit_distance_last_target then begin
  edit_distance_delta := 0;
end else begin
  edit_distance_delta := 1;
end;
  edit_distance_delete_cost := edit_distance(copy(edit_distance_source, 1, (Length(edit_distance_source) - 1 - (0))), edit_distance_target) + 1;
  edit_distance_insert_cost := edit_distance(edit_distance_source, copy(edit_distance_target, 1, (Length(edit_distance_target) - 1 - (0)))) + 1;
  edit_distance_replace_cost := edit_distance(copy(edit_distance_source, 1, (Length(edit_distance_source) - 1 - (0))), copy(edit_distance_target, 1, (Length(edit_distance_target) - 1 - (0)))) + edit_distance_delta;
  exit(min3(edit_distance_delete_cost, edit_distance_insert_cost, edit_distance_replace_cost));
end;
procedure main();
var
  main_result_: integer;
begin
  main_result_ := edit_distance('ATCGCTG', 'TAGCTAA');
  writeln(IntToStr(main_result_));
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
  writeln('');
end.

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
  letters: string;
  title: string;
function excel_title_to_column(title: string): integer; forward;
procedure main(); forward;
function excel_title_to_column(title: string): integer;
var
  excel_title_to_column_result_: integer;
  excel_title_to_column_i: integer;
  excel_title_to_column_ch: string;
  excel_title_to_column_value: integer;
  excel_title_to_column_idx: integer;
  excel_title_to_column_found: boolean;
begin
  excel_title_to_column_result_ := 0;
  excel_title_to_column_i := 0;
  while excel_title_to_column_i < Length(title) do begin
  excel_title_to_column_ch := copy(title, excel_title_to_column_i+1, (excel_title_to_column_i + 1 - (excel_title_to_column_i)));
  excel_title_to_column_value := 0;
  excel_title_to_column_idx := 0;
  excel_title_to_column_found := false;
  while excel_title_to_column_idx < Length(letters) do begin
  if copy(letters, excel_title_to_column_idx+1, (excel_title_to_column_idx + 1 - (excel_title_to_column_idx))) = excel_title_to_column_ch then begin
  excel_title_to_column_value := excel_title_to_column_idx + 1;
  excel_title_to_column_found := true;
  break;
end;
  excel_title_to_column_idx := excel_title_to_column_idx + 1;
end;
  if not excel_title_to_column_found then begin
  panic('title must contain only uppercase A-Z');
end;
  excel_title_to_column_result_ := (excel_title_to_column_result_ * 26) + excel_title_to_column_value;
  excel_title_to_column_i := excel_title_to_column_i + 1;
end;
  exit(excel_title_to_column_result_);
end;
procedure main();
begin
  writeln(excel_title_to_column('A'));
  writeln(excel_title_to_column('B'));
  writeln(excel_title_to_column('AB'));
  writeln(excel_title_to_column('Z'));
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  letters := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  main();
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

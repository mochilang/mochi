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
  pyproject: string;
  project: string;
  toml: string;
function parse_project_name(toml: string): string; forward;
function parse_project_name(toml: string): string;
var
  parse_project_name_i: integer;
  parse_project_name_name: string;
  parse_project_name_n: integer;
begin
  parse_project_name_i := 0;
  parse_project_name_name := '';
  parse_project_name_n := Length(toml);
  while (parse_project_name_i + 4) < parse_project_name_n do begin
  if (((toml[parse_project_name_i+1] = 'n') and (toml[parse_project_name_i + 1+1] = 'a')) and (toml[parse_project_name_i + 2+1] = 'm')) and (toml[parse_project_name_i + 3+1] = 'e') then begin
  parse_project_name_i := parse_project_name_i + 4;
  while (parse_project_name_i < parse_project_name_n) and (toml[parse_project_name_i+1] <> '"') do begin
  parse_project_name_i := parse_project_name_i + 1;
end;
  parse_project_name_i := parse_project_name_i + 1;
  while (parse_project_name_i < parse_project_name_n) and (toml[parse_project_name_i+1] <> '"') do begin
  parse_project_name_name := parse_project_name_name + toml[parse_project_name_i+1];
  parse_project_name_i := parse_project_name_i + 1;
end;
  exit(parse_project_name_name);
end;
  parse_project_name_i := parse_project_name_i + 1;
end;
  exit(parse_project_name_name);
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  pyproject := '[project]' + #10 + 'name = "thealgorithms-python"';
  project := parse_project_name(pyproject);
  writeln(project);
  bench_memdiff_0 := _mem() - bench_mem_0;
  bench_dur_0 := (_bench_now() - bench_start_0) div 1000;
  writeln('{');
  writeln(('  "duration_us": ' + IntToStr(bench_dur_0)) + ',');
  writeln(('  "memory_bytes": ' + IntToStr(bench_memdiff_0)) + ',');
  writeln(('  "name": "' + 'main') + '"');
  writeln('}');
end.

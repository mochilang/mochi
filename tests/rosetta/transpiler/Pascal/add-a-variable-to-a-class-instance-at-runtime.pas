{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
end;
type SomeStruct = record
  runtimeFields: specialize TFPGMap<string, string>;
end;
var
  bench_start_0: integer;
  bench_dur_0: integer;
  bench_mem_0: int64;
  bench_memdiff_0: int64;
  main_ss: SomeStruct;
  main_i: integer;
  main_name: string;
  main_value: string;
  main_fields: specialize TFPGMap<string, string>;
function Map1(): specialize TFPGMap<string, string>; forward;
function makeSomeStruct(runtimeFields: specialize TFPGMap<string, string>): SomeStruct; forward;
procedure main(); forward;
function Map1(): specialize TFPGMap<string, string>;
begin
  Result := specialize TFPGMap<string, string>.Create();
end;
function makeSomeStruct(runtimeFields: specialize TFPGMap<string, string>): SomeStruct;
begin
  Result.runtimeFields := runtimeFields;
end;
procedure main();
begin
  main_ss := makeSomeStruct(Map1());
  writeln('Create two fields at runtime: ' + #10 + '');
  main_i := 1;
  while main_i <= 2 do begin
  writeln(('  Field #' + IntToStr(main_i)) + ':' + #10 + '');
  writeln('       Enter name  : ');
  main_name := _input();
  writeln('       Enter value : ');
  main_value := _input();
  main_fields := main_ss.runtimeFields;
  main_fields.AddOrSetData(main_name, main_value);
  main_ss.runtimeFields := main_fields;
  writeln('' + #10 + '');
  main_i := main_i + 1;
end;
  while true do begin
  writeln('Which field do you want to inspect ? ');
  main_name := _input();
  if main_ss.runtimeFields.IndexOf(main_name) <> -1 then begin
  main_value := main_ss.runtimeFields[main_name];
  writeln(('Its value is ''' + main_value) + '''');
  exit();
end else begin
  writeln('There is no field of that name, try again' + #10 + '');
end;
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

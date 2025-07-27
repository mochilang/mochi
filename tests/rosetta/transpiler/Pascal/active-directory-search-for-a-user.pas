{$mode objfpc}
program Main;
uses SysUtils, fgl;
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
  main_client: specialize TFPGMap<string, Variant>;
  main_directory: specialize TFPGMap<string, StrArray>;
  main_groups: StrArray;
  main_out: string;
  main_i: integer;
function Map2(): specialize TFPGMap<string, StrArray>; forward;
function Map1(): specialize TFPGMap<string, Variant>; forward;
function search_user(directory: specialize TFPGMap<string, StrArray>; username: string): StrArray; forward;
procedure main(); forward;
function Map2(): specialize TFPGMap<string, StrArray>;
begin
  Result := specialize TFPGMap<string, StrArray>.Create();
  Result.AddOrSetData('username', ['admins', 'users']);
  Result.AddOrSetData('john', ['users']);
end;
function Map1(): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('Base', 'dc=example,dc=com');
  Result.AddOrSetData('Host', 'ldap.example.com');
  Result.AddOrSetData('Port', 389);
  Result.AddOrSetData('GroupFilter', '(memberUid=%s)');
end;
function search_user(directory: specialize TFPGMap<string, StrArray>; username: string): StrArray;
begin
  exit(directory[username]);
end;
procedure main();
begin
  main_client := Map1();
  main_directory := Map2();
  main_groups := search_user(main_directory, 'username');
  if Length(main_groups) > 0 then begin
  main_out := 'Groups: [';
  main_i := 0;
  while main_i < Length(main_groups) do begin
  main_out := ((main_out + '"') + main_groups[main_i]) + '"';
  if main_i < (Length(main_groups) - 1) then begin
  main_out := main_out + ', ';
end;
  main_i := main_i + 1;
end;
  main_out := main_out + ']';
  writeln(main_out);
end else begin
  writeln('User not found');
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

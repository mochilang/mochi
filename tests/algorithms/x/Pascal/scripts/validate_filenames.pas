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
  sub: string;
  s: string;
  files: StrArray;
function indexOf(s: string; sub: string): integer; forward;
function contains(s: string; sub: string): boolean; forward;
function validate(files: StrArray): integer; forward;
procedure main(); forward;
function indexOf(s: string; sub: string): integer;
var
  indexOf_n: integer;
  indexOf_m: integer;
  indexOf_i: integer;
begin
  indexOf_n := Length(s);
  indexOf_m := Length(sub);
  indexOf_i := 0;
  while indexOf_i <= (indexOf_n - indexOf_m) do begin
  if copy(s, indexOf_i+1, (indexOf_i + indexOf_m - (indexOf_i))) = sub then begin
  exit(indexOf_i);
end;
  indexOf_i := indexOf_i + 1;
end;
  exit(-1);
end;
function contains(s: string; sub: string): boolean;
begin
  exit(indexOf(s, sub) >= 0);
end;
function validate(files: StrArray): integer;
var
  validate_upper: array of string;
  validate_space: array of string;
  validate_hyphen: array of string;
  validate_nodir: array of string;
  validate_f: string;
begin
  validate_upper := [];
  validate_space := [];
  validate_hyphen := [];
  validate_nodir := [];
  for validate_f in files do begin
  if validate_f <> LowerCase(validate_f) then begin
  validate_upper := concat(validate_upper, StrArray([validate_f]));
end;
  if contains(validate_f, ' ') then begin
  validate_space := concat(validate_space, StrArray([validate_f]));
end;
  if contains(validate_f, '-') and (contains(validate_f, '/site-packages/') = false) then begin
  validate_hyphen := concat(validate_hyphen, StrArray([validate_f]));
end;
  if not contains(validate_f, '/') then begin
  validate_nodir := concat(validate_nodir, StrArray([validate_f]));
end;
end;
  if Length(validate_upper) > 0 then begin
  writeln(IntToStr(Length(validate_upper)) + ' files contain uppercase characters:');
  for validate_f in validate_upper do begin
  writeln(validate_f);
end;
  writeln('');
end;
  if Length(validate_space) > 0 then begin
  writeln(IntToStr(Length(validate_space)) + ' files contain space characters:');
  for validate_f in validate_space do begin
  writeln(validate_f);
end;
  writeln('');
end;
  if Length(validate_hyphen) > 0 then begin
  writeln(IntToStr(Length(validate_hyphen)) + ' files contain hyphen characters:');
  for validate_f in validate_hyphen do begin
  writeln(validate_f);
end;
  writeln('');
end;
  if Length(validate_nodir) > 0 then begin
  writeln(IntToStr(Length(validate_nodir)) + ' files are not in a directory:');
  for validate_f in validate_nodir do begin
  writeln(validate_f);
end;
  writeln('');
end;
  exit(((Length(validate_upper) + Length(validate_space)) + Length(validate_hyphen)) + Length(validate_nodir));
end;
procedure main();
var
  main_files: array of string;
  main_bad: integer;
begin
  main_files := ['scripts/Validate_filenames.py', 'good/file.txt', 'bad file.txt', '/site-packages/pkg-name.py', 'nopath', 'src/hyphen-name.py'];
  main_bad := validate(main_files);
  writeln(IntToStr(main_bad));
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

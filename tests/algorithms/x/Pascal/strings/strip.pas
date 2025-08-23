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
function contains(contains_chars: string; contains_ch: string): boolean; forward;
function substring(substring_s: string; substring_start: integer; substring_end_: integer): string; forward;
function strip_chars(strip_chars_user_string: string; strip_chars_characters: string): string; forward;
function strip(strip_user_string: string): string; forward;
procedure test_strip(); forward;
procedure main(); forward;
function contains(contains_chars: string; contains_ch: string): boolean;
var
  contains_i: integer;
begin
  contains_i := 0;
  while contains_i < Length(contains_chars) do begin
  if contains_chars[contains_i+1] = contains_ch then begin
  exit(true);
end;
  contains_i := contains_i + 1;
end;
  exit(false);
end;
function substring(substring_s: string; substring_start: integer; substring_end_: integer): string;
var
  substring_res: string;
  substring_i: integer;
begin
  substring_res := '';
  substring_i := substring_start;
  while substring_i < substring_end_ do begin
  substring_res := substring_res + substring_s[substring_i+1];
  substring_i := substring_i + 1;
end;
  exit(substring_res);
end;
function strip_chars(strip_chars_user_string: string; strip_chars_characters: string): string;
var
  strip_chars_start: integer;
  strip_chars_end_: integer;
begin
  strip_chars_start := 0;
  strip_chars_end_ := Length(strip_chars_user_string);
  while (strip_chars_start < strip_chars_end_) and contains(strip_chars_characters, strip_chars_user_string[strip_chars_start+1]) do begin
  strip_chars_start := strip_chars_start + 1;
end;
  while (strip_chars_end_ > strip_chars_start) and contains(strip_chars_characters, strip_chars_user_string[strip_chars_end_ - 1+1]) do begin
  strip_chars_end_ := strip_chars_end_ - 1;
end;
  exit(copy(strip_chars_user_string, strip_chars_start+1, (strip_chars_end_ - (strip_chars_start))));
end;
function strip(strip_user_string: string): string;
begin
  exit(strip_chars(strip_user_string, ' 	' + #10 + #13));
end;
procedure test_strip();
begin
  if strip('   hello   ') <> 'hello' then begin
  panic('test1 failed');
end;
  if strip_chars('...world...', '.') <> 'world' then begin
  panic('test2 failed');
end;
  if strip_chars('123hello123', '123') <> 'hello' then begin
  panic('test3 failed');
end;
  if strip('') <> '' then begin
  panic('test4 failed');
end;
end;
procedure main();
begin
  test_strip();
  writeln(strip('   hello   '));
  writeln(strip_chars('...world...', '.'));
  writeln(strip_chars('123hello123', '123'));
  writeln(strip(''));
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

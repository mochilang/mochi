{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type Case_ = record
  text: string;
  expected: boolean;
end;
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
  test_data: array of Case_;
function makeCase_(text: string; expected: boolean): Case_; forward;
function reverse(reverse_s: string): string; forward;
function is_palindrome(is_palindrome_s: string): boolean; forward;
function is_palindrome_traversal(is_palindrome_traversal_s: string): boolean; forward;
function is_palindrome_recursive(is_palindrome_recursive_s: string): boolean; forward;
function is_palindrome_slice(is_palindrome_slice_s: string): boolean; forward;
procedure main(); forward;
function makeCase_(text: string; expected: boolean): Case_;
begin
  Result.text := text;
  Result.expected := expected;
end;
function reverse(reverse_s: string): string;
var
  reverse_res: string;
  reverse_i: int64;
begin
  reverse_res := '';
  reverse_i := Length(reverse_s) - 1;
  while reverse_i >= 0 do begin
  reverse_res := reverse_res + reverse_s[reverse_i+1];
  reverse_i := reverse_i - 1;
end;
  exit(reverse_res);
end;
function is_palindrome(is_palindrome_s: string): boolean;
var
  is_palindrome_start_i: int64;
  is_palindrome_end_i: int64;
begin
  is_palindrome_start_i := 0;
  is_palindrome_end_i := Length(is_palindrome_s) - 1;
  while is_palindrome_start_i < is_palindrome_end_i do begin
  if is_palindrome_s[is_palindrome_start_i+1] = is_palindrome_s[is_palindrome_end_i+1] then begin
  is_palindrome_start_i := is_palindrome_start_i + 1;
  is_palindrome_end_i := is_palindrome_end_i - 1;
end else begin
  exit(false);
end;
end;
  exit(true);
end;
function is_palindrome_traversal(is_palindrome_traversal_s: string): boolean;
var
  is_palindrome_traversal_end_: int64;
  is_palindrome_traversal_n: int64;
  is_palindrome_traversal_i: int64;
begin
  is_palindrome_traversal_end_ := Length(is_palindrome_traversal_s) div 2;
  is_palindrome_traversal_n := Length(is_palindrome_traversal_s);
  is_palindrome_traversal_i := 0;
  while is_palindrome_traversal_i < is_palindrome_traversal_end_ do begin
  if is_palindrome_traversal_s[is_palindrome_traversal_i+1] <> is_palindrome_traversal_s[(is_palindrome_traversal_n - is_palindrome_traversal_i) - 1+1] then begin
  exit(false);
end;
  is_palindrome_traversal_i := is_palindrome_traversal_i + 1;
end;
  exit(true);
end;
function is_palindrome_recursive(is_palindrome_recursive_s: string): boolean;
begin
  if Length(is_palindrome_recursive_s) <= 1 then begin
  exit(true);
end;
  if is_palindrome_recursive_s[0+1] = is_palindrome_recursive_s[Length(is_palindrome_recursive_s) - 1+1] then begin
  exit(is_palindrome_recursive(copy(is_palindrome_recursive_s, 2, (Length(is_palindrome_recursive_s) - 1 - (1)))));
end;
  exit(false);
end;
function is_palindrome_slice(is_palindrome_slice_s: string): boolean;
begin
  exit(is_palindrome_slice_s = reverse(is_palindrome_slice_s));
end;
procedure main();
var
  main_t: Case_;
  main_s: string;
  main_expected: boolean;
  main_r1: boolean;
  main_r2: boolean;
  main_r3: boolean;
  main_r4: boolean;
begin
  for main_t in test_data do begin
  main_s := main_t.text;
  main_expected := main_t.expected;
  main_r1 := is_palindrome(main_s);
  main_r2 := is_palindrome_traversal(main_s);
  main_r3 := is_palindrome_recursive(main_s);
  main_r4 := is_palindrome_slice(main_s);
  if (((main_r1 <> main_expected) or (main_r2 <> main_expected)) or (main_r3 <> main_expected)) or (main_r4 <> main_expected) then begin
  panic('algorithm mismatch');
end;
  writeln((main_s + ' ') + LowerCase(BoolToStr(main_expected, true)));
end;
  writeln('a man a plan a canal panama');
end;
begin
  init_now();
  bench_mem_0 := _mem();
  bench_start_0 := _bench_now();
  test_data := [makeCase_('MALAYALAM', true), makeCase_('String', false), makeCase_('rotor', true), makeCase_('level', true), makeCase_('A', true), makeCase_('BB', true), makeCase_('ABC', false), makeCase_('amanaplanacanalpanama', true)];
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

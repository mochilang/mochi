{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
type IntArray = array of int64;
type StrArray = array of string;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
procedure error(msg: string);
begin
  panic(msg);
end;
function _floordiv(a, b: int64): int64; var r: int64;
begin
  r := a div b;
  if ((a < 0) xor (b < 0)) and ((a mod b) <> 0) then r := r - 1;
  _floordiv := r;
end;
function _to_float(x: int64): real;
begin
  _to_float := x;
end;
function to_float(x: int64): real;
begin
  to_float := _to_float(x);
end;
procedure json(xs: array of real); overload;
var i: integer;
begin
  write('[');
  for i := 0 to High(xs) do begin
    write(xs[i]);
    if i < High(xs) then write(', ');
  end;
  writeln(']');
end;
procedure json(x: int64); overload;
begin
  writeln(x);
end;
var
  sample_input: array of string;
  r: int64;
function is_substring(is_substring_a: string; is_substring_b: string): int64; forward;
function solve(solve_lines: StrArray): IntArray; forward;
function is_substring(is_substring_a: string; is_substring_b: string): int64;
var
  is_substring_la: integer;
  is_substring_lb: integer;
  is_substring_i: int64;
begin
  is_substring_la := Length(is_substring_a);
  is_substring_lb := Length(is_substring_b);
  is_substring_i := 0;
  while (is_substring_i + is_substring_lb) <= is_substring_la do begin
  if copy(is_substring_a, is_substring_i+1, (is_substring_i + is_substring_lb - (is_substring_i))) = is_substring_b then begin
  exit(1);
end;
  is_substring_i := is_substring_i + 1;
end;
  exit(0);
end;
function solve(solve_lines: StrArray): IntArray;
var
  solve_res: array of int64;
  solve_line: string;
  solve_parts: array of string;
  solve_cur: string;
  solve_i: int64;
  solve_ch: string;
  solve_a: string;
  solve_b: string;
begin
  solve_res := [];
  for solve_line in solve_lines do begin
  solve_parts := [];
  solve_cur := '';
  solve_i := 0;
  while solve_i < Length(solve_line) do begin
  solve_ch := copy(solve_line, solve_i+1, (solve_i + 1 - (solve_i)));
  if solve_ch = ' ' then begin
  solve_parts := concat(solve_parts, StrArray([solve_cur]));
  solve_cur := '';
end else begin
  solve_cur := solve_cur + solve_ch;
end;
  solve_i := solve_i + 1;
end;
  solve_parts := concat(solve_parts, StrArray([solve_cur]));
  solve_a := solve_parts[0];
  solve_b := solve_parts[1];
  solve_res := concat(solve_res, IntArray([is_substring(solve_a, solve_b)]));
end;
  exit(solve_res);
end;
begin
  sample_input := ['1010110010 10110', '1110111011 10011'];
  for r in solve(sample_input) do begin
  writeln(r);
end;
end.

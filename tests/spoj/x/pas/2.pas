{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type IntArray = array of int64;
type StrArray = array of string;
type BoolArray = array of boolean;
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
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
  primes: IntArray;
  t: integer;
  case_idx: int64;
  line: string;
  parts: StrArray;
  m: integer;
  n: integer;
  size: integer;
  segment: array of boolean;
  i: int64;
  p: int64;
  start: int64;
  rem: integer;
  j: int64;
function split(split_s: string; split_sep: string): StrArray; forward;
function precompute(precompute_limit: int64): IntArray; forward;
function split(split_s: string; split_sep: string): StrArray;
var
  split_parts: array of string;
  split_cur: string;
  split_i: int64;
begin
  split_parts := [];
  split_cur := '';
  split_i := 0;
  while split_i < Length(split_s) do begin
  if ((Length(split_sep) > 0) and ((split_i + Length(split_sep)) <= Length(split_s))) and (copy(split_s, split_i+1, (split_i + Length(split_sep) - (split_i))) = split_sep) then begin
  split_parts := concat(split_parts, StrArray([split_cur]));
  split_cur := '';
  split_i := split_i + Length(split_sep);
end else begin
  split_cur := split_cur + copy(split_s, split_i+1, (split_i + 1 - (split_i)));
  split_i := split_i + 1;
end;
end;
  split_parts := concat(split_parts, StrArray([split_cur]));
  exit(split_parts);
end;
function precompute(precompute_limit: int64): IntArray;
var
  precompute_sieve: array of boolean;
  precompute_i: int64;
  precompute_p: int64;
  precompute_j: int64;
  precompute_primes: array of int64;
begin
  precompute_sieve := [];
  for precompute_i := 0 to (precompute_limit + 1 - 1) do begin
  precompute_sieve := concat(precompute_sieve, [true]);
end;
  precompute_sieve[0] := false;
  precompute_sieve[1] := false;
  precompute_p := 2;
  while (precompute_p * precompute_p) <= precompute_limit do begin
  if precompute_sieve[precompute_p] then begin
  precompute_j := precompute_p * precompute_p;
  while precompute_j <= precompute_limit do begin
  precompute_sieve[precompute_j] := false;
  precompute_j := precompute_j + precompute_p;
end;
end;
  precompute_p := precompute_p + 1;
end;
  precompute_primes := [];
  for precompute_i := 2 to (precompute_limit + 1 - 1) do begin
  if precompute_sieve[precompute_i] then begin
  precompute_primes := concat(precompute_primes, IntArray([precompute_i]));
end;
end;
  exit(precompute_primes);
end;
begin
  primes := precompute(32000);
  t := StrToInt(_input());
  case_idx := 0;
  while case_idx < t do begin
  line := _input();
  parts := split(line, ' ');
  m := StrToInt(parts[0]);
  n := StrToInt(parts[1]);
  size := (n - m) + 1;
  segment := [];
  for i := 0 to (size - 1) do begin
  segment := concat(segment, [true]);
end;
  for p in primes do begin
  if (p * p) > n then begin
  break;
end;
  start := p * p;
  if start < m then begin
  rem := m mod p;
  if rem = 0 then begin
  start := m;
end else begin
  start := m + (p - rem);
end;
end;
  j := start;
  while j <= n do begin
  segment[j - m] := false;
  j := j + p;
end;
end;
  if m = 1 then begin
  segment[0] := false;
end;
  i := 0;
  while i < size do begin
  if segment[i] then begin
  writeln(i + m);
end;
  i := i + 1;
end;
  if case_idx < (t - 1) then begin
  writeln('');
end;
  case_idx := case_idx + 1;
end;
end.

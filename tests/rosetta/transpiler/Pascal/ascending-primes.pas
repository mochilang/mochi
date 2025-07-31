{$mode objfpc}
program Main;
uses SysUtils;
var
  asc: array of integer;
function isPrime(n: integer): boolean; forward;
procedure gen(first: integer; cand: integer; digits: integer); forward;
function pad(n: integer; width: integer): string; forward;
procedure main(); forward;
function isPrime(n: integer): boolean;
var
  isPrime_d: integer;
begin
  if n < 2 then begin
  exit(false);
end;
  if (n mod 2) = 0 then begin
  exit(n = 2);
end;
  if (n mod 3) = 0 then begin
  exit(n = 3);
end;
  isPrime_d := 5;
  while (isPrime_d * isPrime_d) <= n do begin
  if (n mod isPrime_d) = 0 then begin
  exit(false);
end;
  isPrime_d := isPrime_d + 2;
  if (n mod isPrime_d) = 0 then begin
  exit(false);
end;
  isPrime_d := isPrime_d + 4;
end;
  exit(true);
end;
procedure gen(first: integer; cand: integer; digits: integer);
var
  gen_i: integer;
begin
  if digits = 0 then begin
  if isPrime(cand) then begin
  asc := concat(asc, [cand]);
end;
  exit();
end;
  gen_i := first;
  while gen_i < 10 do begin
  gen(gen_i + 1, (cand * 10) + gen_i, digits - 1);
  gen_i := gen_i + 1;
end;
end;
function pad(n: integer; width: integer): string;
var
  pad_s: string;
begin
  pad_s := IntToStr(n);
  while Length(pad_s) < width do begin
  pad_s := ' ' + pad_s;
end;
  exit(pad_s);
end;
procedure main();
var
  main_digits: integer;
  main_i: integer;
  main_line: string;
begin
  main_digits := 1;
  while main_digits < 10 do begin
  gen(1, 0, main_digits);
  main_digits := main_digits + 1;
end;
  writeln(('There are ' + IntToStr(Length(asc))) + ' ascending primes, namely:');
  main_i := 0;
  main_line := '';
  while main_i < Length(asc) do begin
  main_line := (main_line + pad(asc[main_i], 8)) + ' ';
  if ((main_i + 1) mod 10) = 0 then begin
  writeln(copy(main_line, 0+1, (Length(main_line) - 1 - (0))));
  main_line := '';
end;
  main_i := main_i + 1;
end;
  if Length(main_line) > 0 then begin
  writeln(copy(main_line, 0+1, (Length(main_line) - 1 - (0))));
end;
end;
begin
  asc := [];
  main();
end.

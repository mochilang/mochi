{$mode objfpc}
program Main;
uses SysUtils;
function isPrime(n: integer): boolean; forward;
function sumDigits(n: integer): integer; forward;
function pad(n: integer): string; forward;
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
function sumDigits(n: integer): integer;
var
  sumDigits_s: integer;
  sumDigits_x: integer;
begin
  sumDigits_s := 0;
  sumDigits_x := n;
  while sumDigits_x > 0 do begin
  sumDigits_s := sumDigits_s + (sumDigits_x mod 10);
  sumDigits_x := Trunc(sumDigits_x div 10);
end;
  exit(sumDigits_s);
end;
function pad(n: integer): string;
begin
  if n < 10 then begin
  exit('  ' + IntToStr(n));
end;
  if n < 100 then begin
  exit(' ' + IntToStr(n));
end;
  exit(IntToStr(n));
end;
procedure main();
var
  main_count: integer;
  main_line: string;
  main_lineCount: integer;
  main_i: integer;
begin
  writeln('Additive primes less than 500:');
  main_count := 0;
  main_line := '';
  main_lineCount := 0;
  main_i := 2;
  while main_i < 500 do begin
  if isPrime(main_i) and isPrime(sumDigits(main_i)) then begin
  main_count := main_count + 1;
  main_line := (main_line + pad(main_i)) + '  ';
  main_lineCount := main_lineCount + 1;
  if main_lineCount = 10 then begin
  writeln(copy(main_line, 0+1, (Length(main_line) - 2 - (0))));
  main_line := '';
  main_lineCount := 0;
end;
end;
  if main_i > 2 then begin
  main_i := main_i + 2;
end else begin
  main_i := main_i + 1;
end;
end;
  if main_lineCount > 0 then begin
  writeln(copy(main_line, 0+1, (Length(main_line) - 2 - (0))));
end;
  writeln(IntToStr(main_count) + ' additive primes found.');
end;
begin
  main();
end.

{$mode objfpc}
program Main;
uses SysUtils;
procedure panic(msg: string);
begin
  writeln(msg);
  halt(1);
end;
function isPrime(isPrime_number: integer): boolean; forward;
function nextPrime(nextPrime_value: integer; nextPrime_factor: integer; nextPrime_desc: boolean): integer; forward;
function isPrime(isPrime_number: integer): boolean;
var
  isPrime_i: integer;
begin
  if isPrime_number < 2 then begin
  exit(false);
end;
  if isPrime_number < 4 then begin
  exit(true);
end;
  if (isPrime_number mod 2) = 0 then begin
  exit(false);
end;
  isPrime_i := 3;
  while (isPrime_i * isPrime_i) <= isPrime_number do begin
  if (isPrime_number mod isPrime_i) = 0 then begin
  exit(false);
end;
  isPrime_i := isPrime_i + 2;
end;
  exit(true);
end;
function nextPrime(nextPrime_value: integer; nextPrime_factor: integer; nextPrime_desc: boolean): integer;
var
  nextPrime_v: integer;
  nextPrime_firstValue: integer;
begin
  nextPrime_v := nextPrime_value * nextPrime_factor;
  nextPrime_firstValue := nextPrime_v;
  while not isPrime(nextPrime_v) do begin
  if nextPrime_desc then begin
  nextPrime_v := nextPrime_v - 1;
end else begin
  nextPrime_v := nextPrime_v + 1;
end;
end;
  if nextPrime_v = nextPrime_firstValue then begin
  if nextPrime_desc then begin
  exit(nextPrime(nextPrime_v - 1, 1, nextPrime_desc));
end else begin
  exit(nextPrime(nextPrime_v + 1, 1, nextPrime_desc));
end;
end;
  exit(nextPrime_v);
end;
begin
  writeln(Ord(isPrime(0)));
  writeln(Ord(isPrime(1)));
  writeln(Ord(isPrime(2)));
  writeln(Ord(isPrime(3)));
  writeln(Ord(isPrime(27)));
  writeln(Ord(isPrime(87)));
  writeln(Ord(isPrime(563)));
  writeln(Ord(isPrime(2999)));
  writeln(Ord(isPrime(67483)));
  writeln(nextPrime(14, 1, false));
  writeln(nextPrime(14, 1, true));
end.

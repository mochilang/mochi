{$mode objfpc}
program Main;
var
  PI: real;
  sinApprox_term: float;
  sinApprox_sum: float;
  sinApprox_n: integer;
  sinApprox_denom: integer;
  dt: real;
  s: float;
  t1: real;
  k1: float;
  i: integer;
  t2: real;
  k2: float;
  i2: integer;
  t2_13: real;
  k2_14: real;
function sinApprox(x: float): float; forward;
function sinApprox(x: float): float;
begin
  sinApprox_term := x;
  sinApprox_sum := x;
  sinApprox_n := 1;
  while sinApprox_n <= 12 do begin
  sinApprox_denom := (2 * sinApprox_n) * ((2 * sinApprox_n) + 1);
  sinApprox_term := ((-sinApprox_term * x) * x) div sinApprox_denom;
  sinApprox_sum := sinApprox_sum + sinApprox_term;
  sinApprox_n := sinApprox_n + 1;
end;
  exit(sinApprox_sum);
end;
begin
  PI := 3.141592653589793;
  dt := 0.01;
  s := 0;
  t1 := 0;
  k1 := sinApprox(0);
  i := 1;
  i2 := 1;
  while i <= 200 do begin
  t2 := i * dt;
  k2 := sinApprox(t2 * PI);
  s := s + (((k1 + k2) * 0.5) * (t2 - t1));
  t1 := t2;
  k1 := k2;
  i := i + 1;
end;
  while i2 <= 50 do begin
  t2_13 := 2 + (i2 * dt);
  k2_14 := 0;
  s := s + (((k1 + k2_14) * 0.5) * (t2_13 - t1));
  t1 := t2_13;
  k1 := k2_14;
  i2 := i2 + 1;
end;
  writeln(s);
end.

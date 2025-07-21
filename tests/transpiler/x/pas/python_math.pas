{$mode objfpc}
program Main;
uses Math;
var
  r: real;
  area: real;
  root: real;
  sin45: real;
  log_e: real;
begin
  r := 3;
  area := Pi * Power(r, 2);
  root := Sqrt(49);
  sin45 := Sin(Pi / 4);
  log_e := Ln(2.718281828459045);
  writeln('Circle area with r =', ' ', r:0:1, ' ', '=>', ' ', area:0:1);
  writeln('Square root of 49:', ' ', root:0:1);
  writeln('sin(π/4):', ' ', sin45:0:1);
  writeln('log(e):', ' ', log_e:0:1);
end.

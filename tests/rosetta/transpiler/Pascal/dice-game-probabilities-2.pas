{$mode objfpc}
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
function roll(nDice: integer; nSides: integer): integer; forward;
function beats(n1: integer; s1: integer; n2: integer; s2: integer; trials: integer): real; forward;
function roll(nDice: integer; nSides: integer): integer;
var
  roll_sum: integer;
  roll_i: integer;
begin
  roll_sum := 0;
  roll_i := 0;
  while roll_i < nDice do begin
  roll_sum := (roll_sum + (_now() mod nSides)) + 1;
  roll_i := roll_i + 1;
end;
  exit(roll_sum);
end;
function beats(n1: integer; s1: integer; n2: integer; s2: integer; trials: integer): real;
var
  beats_wins: integer;
  beats_i: integer;
begin
  beats_wins := 0;
  beats_i := 0;
  while beats_i < trials do begin
  if roll(n1, s1) > roll(n2, s2) then begin
  beats_wins := beats_wins + 1;
end;
  beats_i := beats_i + 1;
end;
  exit(Double(beats_wins) / Double(trials));
end;
begin
  init_now();
  writeln(FloatToStr(beats(9, 4, 6, 6, 1000)));
  writeln(FloatToStr(beats(5, 10, 7, 6, 1000)));
end.

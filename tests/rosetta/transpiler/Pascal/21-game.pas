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
function _input(): string;
var s: string;
begin
  if EOF(Input) then s := '' else ReadLn(s);
  _input := s;
end;
type Anon1 = record
  f0: integer;
  f1: integer;
  f2: integer;
  f3: integer;
  f4: integer;
  f5: integer;
  f6: integer;
  f7: integer;
  f8: integer;
  f9: integer;
end;
var
  parseIntStr_i: integer;
  parseIntStr_neg: boolean;
  parseIntStr_n: integer;
  parseIntStr_digits: Anon1;
  main_total: integer;
  main_computer: boolean;
  main_round: integer;
  main_done: boolean;
  main_i: integer;
  main_choice: integer;
  main_line: string;
  main_num: integer;
function makeAnon1(f0: integer; f1: integer; f2: integer; f3: integer; f4: integer; f5: integer; f6: integer; f7: integer; f8: integer; f9: integer): Anon1;
begin
  Result.f0 := f0;
  Result.f1 := f1;
  Result.f2 := f2;
  Result.f3 := f3;
  Result.f4 := f4;
  Result.f5 := f5;
  Result.f6 := f6;
  Result.f7 := f7;
  Result.f8 := f8;
  Result.f9 := f9;
end;
function parseIntStr(str: string): integer;
begin
  exit(StrToInt(str));
end;
procedure main();
begin
  main_total := 0;
  main_computer := (_now() mod 2) = 0;
  writeln('Enter q to quit at any time' + #10 + '');
  if main_computer then begin
  writeln('The computer will choose first');
end else begin
  writeln('You will choose first');
end;
  writeln('' + #10 + '' + #10 + 'Running total is now 0' + #10 + '' + #10 + '');
  main_round := 1;
  main_done := false;
  while not main_done do begin
  writeln(('ROUND ' + IntToStr(main_round)) + ':' + #10 + '' + #10 + '');
  main_i := 0;
  while (main_i < 2) and not main_done do begin
  if main_computer then begin
  main_choice := 0;
  if main_total < 18 then begin
  main_choice := (_now() mod 3) + 1;
end else begin
  main_choice := 21 - main_total;
end;
  main_total := main_total + main_choice;
  writeln('The computer chooses ' + IntToStr(main_choice));
  writeln('Running total is now ' + IntToStr(main_total));
  if main_total = 21 then begin
  writeln('' + #10 + 'So, commiserations, the computer has won!');
  main_done := true;
end;
end else begin
  while true do begin
  writeln('Your choice 1 to 3 : ');
  main_line := _input();
  if (main_line = 'q') or (main_line = 'Q') then begin
  writeln('OK, quitting the game');
  main_done := true;
  break;
end;
  main_num := parseIntStr(main_line);
  if (main_num < 1) or (main_num > 3) then begin
  if (main_total + main_num) > 21 then begin
  writeln('Too big, try again');
end else begin
  writeln('Out of range, try again');
end;
  continue;
end;
  if (main_total + main_num) > 21 then begin
  writeln('Too big, try again');
  continue;
end;
  main_total := main_total + main_num;
  writeln('Running total is now ' + IntToStr(main_total));
  break;
end;
  if main_total = 21 then begin
  writeln('' + #10 + 'So, congratulations, you''ve won!');
  main_done := true;
end;
end;
  writeln('' + #10 + '');
  main_computer := not main_computer;
  main_i := main_i + 1;
end;
  main_round := main_round + 1;
end;
end;
begin
  init_now();
  main();
end.

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
    _now := Integer(GetTickCount64());
  end;
end;
type IntArray = array of integer;
var
  arr: IntArray;
  i: integer;
  j: integer;
  tmp: integer;
  pardoned: integer;
  t: integer;
  drawers: array of integer;
  p: integer;
  success: boolean;
  found: boolean;
  prev: integer;
  d: integer;
  this: integer;
  opened: array of boolean;
  k: integer;
  n: integer;
  rf: integer;
  trials: integer;
  strat: string;
  np: integer;
function shuffle(xs: IntArray): IntArray;
begin
  arr := xs;
  i := 99;
  while i > 0 do begin
  j := _now() mod (i + 1);
  tmp := arr[i];
  arr[i] := arr[j];
  arr[j] := tmp;
  i := i - 1;
end;
  exit(arr);
end;
procedure doTrials(trials: integer; np: integer; strategy: string);
begin
  pardoned := 0;
  t := 0;
  while t < trials do begin
  drawers := [];
  i := 0;
  while i < 100 do begin
  drawers := concat(drawers, [i]);
  i := i + 1;
end;
  drawers := shuffle(drawers);
  p := 0;
  success := true;
  while p < np do begin
  found := false;
  if strategy = 'optimal' then begin
  prev := p;
  d := 0;
  while d < 50 do begin
  this := drawers[prev];
  if this = p then begin
  found := true;
  break;
end;
  prev := this;
  d := d + 1;
end;
end else begin
  opened := [];
  k := 0;
  while k < 100 do begin
  opened := concat(opened, [false]);
  k := k + 1;
end;
  d := 0;
  while d < 50 do begin
  n := _now() mod 100;
  while opened[n] do begin
  n := _now() mod 100;
end;
  opened[n] := true;
  if drawers[n] = p then begin
  found := true;
  break;
end;
  d := d + 1;
end;
end;
  if not found then begin
  success := false;
  break;
end;
  p := p + 1;
end;
  if success then begin
  pardoned := pardoned + 1;
end;
  t := t + 1;
end;
  rf := (pardoned div trials) * 100;
  writeln(((((('  strategy = ' + strategy) + '  pardoned = ') + IntToStr(pardoned)) + ' relative frequency = ') + IntToStr(rf)) + '%');
end;
procedure main();
begin
  trials := 1000;
  for np in [10, 100] do begin
  writeln(((('Results from ' + IntToStr(trials)) + ' trials with ') + IntToStr(np)) + ' prisoners:' + #10 + '');
  for strat in ['random', 'optimal'] do begin
  doTrials(trials, np, strat);
end;
end;
end;
begin
  init_now();
  main();
end.

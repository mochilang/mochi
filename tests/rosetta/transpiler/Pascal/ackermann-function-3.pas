{$mode objfpc}
program Main;
uses SysUtils;
var
  pow_big_result: bigint;
  pow_big_b: bigint;
  pow_big_e: integer;
  bit_len_n: bigint;
  bit_len_c: integer;
  err: string;
  ackermann2_mi: integer;
  ackermann2_nb: integer;
  ackermann2_r: bigint;
  show_res: bigint;
  show_s: string;
  show_pre: string;
  show_suf: string;
function pow_big(base: bigint; exp: integer): bigint; forward;
function bit_len(x: bigint): integer; forward;
function ackermann2(m: bigint; n: bigint): bigint; forward;
procedure show(m: integer; n: integer); forward;
procedure main(); forward;
function pow_big(base: bigint; exp: integer): bigint;
begin
  pow_big_result := 1;
  pow_big_b := base;
  pow_big_e := exp;
  while pow_big_e > 0 do begin
  if (pow_big_e mod 2) = 1 then begin
  pow_big_result := pow_big_result * pow_big_b;
end;
  pow_big_b := pow_big_b * pow_big_b;
  pow_big_e := Trunc(pow_big_e div 2);
end;
  exit(pow_big_result);
end;
function bit_len(x: bigint): integer;
begin
  bit_len_n := x;
  bit_len_c := 0;
  while bit_len_n > 0 do begin
  bit_len_n := bit_len_n div 2;
  bit_len_c := bit_len_c + 1;
end;
  exit(bit_len_c);
end;
function ackermann2(m: bigint; n: bigint): bigint;
begin
  if err <> '' then begin
  exit(0);
end;
  if m <= 3 then begin
  ackermann2_mi := Trunc(m);
  if ackermann2_mi = 0 then begin
  exit(n + 1);
end;
  if ackermann2_mi = 1 then begin
  exit(n + 2);
end;
  if ackermann2_mi = 2 then begin
  exit((2 * n) + 3);
end;
  if ackermann2_mi = 3 then begin
  ackermann2_nb := bit_len(n);
  if ackermann2_nb > 64 then begin
  err := ('A(m,n) had n of ' + IntToStr(ackermann2_nb)) + ' bits; too large';
  exit(0);
end;
  ackermann2_r := pow_big(2, Trunc(n));
  exit((8 * ackermann2_r) - 3);
end;
end;
  if bit_len(n) = 0 then begin
  exit(ackermann2(m - 1, 1));
end;
  exit(ackermann2(m - 1, ackermann2(m, n - 1)));
end;
procedure show(m: integer; n: integer);
begin
  err := '';
  show_res := ackermann2(m, n);
  if err <> '' then begin
  writeln((((('A(' + IntToStr(m)) + ', ') + IntToStr(n)) + ') = Error: ') + err);
  exit();
end;
  if bit_len(show_res) <= 256 then begin
  writeln((((('A(' + IntToStr(m)) + ', ') + IntToStr(n)) + ') = ') + IntToStr(show_res));
end else begin
  show_s := IntToStr(show_res);
  show_pre := copy(show_s, 0+1, (20 - (0)));
  show_suf := copy(show_s, Length(show_s) - 20+1, (Length(show_s) - (Length(show_s) - 20)));
  writeln((((((((('A(' + IntToStr(m)) + ', ') + IntToStr(n)) + ') = ') + IntToStr(Length(show_s))) + ' digits starting/ending with: ') + show_pre) + '...') + show_suf);
end;
end;
procedure main();
begin
  show(0, 0);
  show(1, 2);
  show(2, 4);
  show(3, 100);
  show(3, 1000000);
  show(4, 1);
  show(4, 2);
  show(4, 3);
end;
begin
  err := '';
  main();
end.

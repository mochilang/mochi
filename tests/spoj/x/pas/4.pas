{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils;
type StrArray = array of string;
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
function precedence(precedence_op: string): int64; forward;
function popTop(popTop_stack: StrArray): string; forward;
function popStack(popStack_stack: StrArray): StrArray; forward;
function toRPN(toRPN_expr: string): string; forward;
procedure main(); forward;
function precedence(precedence_op: string): int64;
begin
  if (precedence_op = '+') or (precedence_op = '-') then begin
  exit(1);
end;
  if (precedence_op = '*') or (precedence_op = '/') then begin
  exit(2);
end;
  if precedence_op = '^' then begin
  exit(3);
end;
  exit(0);
end;
function popTop(popTop_stack: StrArray): string;
begin
  exit(popTop_stack[Length(popTop_stack) - 1]);
end;
function popStack(popStack_stack: StrArray): StrArray;
var
  popStack_newStack: array of string;
  popStack_i: int64;
begin
  popStack_newStack := [];
  popStack_i := 0;
  while popStack_i < (Length(popStack_stack) - 1) do begin
  popStack_newStack := concat(popStack_newStack, StrArray([popStack_stack[popStack_i]]));
  popStack_i := popStack_i + 1;
end;
  exit(popStack_newStack);
end;
function toRPN(toRPN_expr: string): string;
var
  toRPN_out_: string;
  toRPN_stack: array of string;
  toRPN_i: int64;
  toRPN_ch: string;
  toRPN_top: string;
  toRPN_prec: int64;
  toRPN_top_6: string;
  toRPN_topPrec: int64;
  toRPN_top_8: string;
begin
  toRPN_out_ := '';
  toRPN_stack := [];
  toRPN_i := 0;
  while toRPN_i < Length(toRPN_expr) do begin
  toRPN_ch := toRPN_expr[toRPN_i+1];
  if (toRPN_ch >= 'a') and (toRPN_ch <= 'z') then begin
  toRPN_out_ := toRPN_out_ + toRPN_ch;
end else begin
  if toRPN_ch = '(' then begin
  toRPN_stack := concat(toRPN_stack, StrArray([toRPN_ch]));
end else begin
  if toRPN_ch = ')' then begin
  while Length(toRPN_stack) > 0 do begin
  toRPN_top := popTop(toRPN_stack);
  if toRPN_top = '(' then begin
  toRPN_stack := popStack(toRPN_stack);
  break;
end;
  toRPN_out_ := toRPN_out_ + toRPN_top;
  toRPN_stack := popStack(toRPN_stack);
end;
end else begin
  toRPN_prec := precedence(toRPN_ch);
  while Length(toRPN_stack) > 0 do begin
  toRPN_top_6 := popTop(toRPN_stack);
  if toRPN_top_6 = '(' then begin
  break;
end;
  toRPN_topPrec := precedence(toRPN_top_6);
  if (toRPN_topPrec > toRPN_prec) or ((toRPN_topPrec = toRPN_prec) and (toRPN_ch <> '^')) then begin
  toRPN_out_ := toRPN_out_ + toRPN_top_6;
  toRPN_stack := popStack(toRPN_stack);
end else begin
  break;
end;
end;
  toRPN_stack := concat(toRPN_stack, StrArray([toRPN_ch]));
end;
end;
end;
  toRPN_i := toRPN_i + 1;
end;
  while Length(toRPN_stack) > 0 do begin
  toRPN_top_8 := popTop(toRPN_stack);
  toRPN_out_ := toRPN_out_ + toRPN_top_8;
  toRPN_stack := popStack(toRPN_stack);
end;
  exit(toRPN_out_);
end;
procedure main();
var
  main_t: int64;
  main_i: int64;
  main_expr: string;
begin
  main_t := StrToInt(_input());
  main_i := 0;
  while main_i < main_t do begin
  main_expr := _input();
  writeln(toRPN(main_expr));
  main_i := main_i + 1;
end;
end;
begin
  main();
end.

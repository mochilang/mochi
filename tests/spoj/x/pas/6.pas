{$mode objfpc}{$modeswitch nestedprocvars}
program Main;
uses SysUtils, Variants, fgl;
type VariantArray = array of Variant;
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
var
  digitMap: specialize TFPGMap<string, int64>;
function Map3(mul_str_parts: VariantArray; mul_str_result_: string): specialize TFPGMap<string, Variant>; forward;
function Map2(mul_str_part: string; mul_str_shift: int64): specialize TFPGMap<string, Variant>; forward;
function Map1(): specialize TFPGMap<string, int64>; forward;
function repeat_(repeat__s: string; repeat__n: int64): string; forward;
function add_str(add_str_a: string; add_str_b: string): string; forward;
function sub_str(sub_str_a: string; sub_str_b: string): string; forward;
function mul_digit(mul_digit_a: string; mul_digit_d: int64): string; forward;
function mul_str(mul_str_a: string; mul_str_b: string): specialize TFPGMap<string, Variant>; forward;
function pad_left(pad_left_s: string; pad_left_total: int64): string; forward;
procedure main(); forward;
function Map3(mul_str_parts: VariantArray; mul_str_result_: string): specialize TFPGMap<string, Variant>;
var
  _ptr4: ^VariantArray;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('res', Variant(mul_str_result_));
  New(_ptr4);
  _ptr4^ := mul_str_parts;
  Result.AddOrSetData('parts', Variant(PtrUInt(_ptr4)));
end;
function Map2(mul_str_part: string; mul_str_shift: int64): specialize TFPGMap<string, Variant>;
begin
  Result := specialize TFPGMap<string, Variant>.Create();
  Result.AddOrSetData('val', Variant(mul_str_part));
  Result.AddOrSetData('shift', Variant(mul_str_shift));
end;
function Map1(): specialize TFPGMap<string, int64>;
begin
  Result := specialize TFPGMap<string, int64>.Create();
  Result.AddOrSetData('0', 0);
  Result.AddOrSetData('1', 1);
  Result.AddOrSetData('2', 2);
  Result.AddOrSetData('3', 3);
  Result.AddOrSetData('4', 4);
  Result.AddOrSetData('5', 5);
  Result.AddOrSetData('6', 6);
  Result.AddOrSetData('7', 7);
  Result.AddOrSetData('8', 8);
  Result.AddOrSetData('9', 9);
end;
function repeat_(repeat__s: string; repeat__n: int64): string;
var
  repeat__r: string;
  repeat___: int64;
begin
  repeat__r := '';
  for repeat___ := 0 to (repeat__n - 1) do begin
  repeat__r := repeat__r + repeat__s;
end;
  exit(repeat__r);
end;
function add_str(add_str_a: string; add_str_b: string): string;
var
  add_str_i: integer;
  add_str_j: integer;
  add_str_carry: int64;
  add_str_res: string;
  add_str_da: int64;
  add_str_db: int64;
  add_str_sum: int64;
begin
  add_str_i := Length(add_str_a) - 1;
  add_str_j := Length(add_str_b) - 1;
  add_str_carry := 0;
  add_str_res := '';
  while ((add_str_i >= 0) or (add_str_j >= 0)) or (add_str_carry > 0) do begin
  add_str_da := 0;
  if add_str_i >= 0 then begin
  add_str_da := Trunc(digitMap[copy(add_str_a, add_str_i+1, (add_str_i + 1 - (add_str_i)))]);
end;
  add_str_db := 0;
  if add_str_j >= 0 then begin
  add_str_db := Trunc(digitMap[copy(add_str_b, add_str_j+1, (add_str_j + 1 - (add_str_j)))]);
end;
  add_str_sum := (add_str_da + add_str_db) + add_str_carry;
  add_str_res := IntToStr(add_str_sum mod 10) + add_str_res;
  add_str_carry := _floordiv(add_str_sum, 10);
  add_str_i := add_str_i - 1;
  add_str_j := add_str_j - 1;
end;
  exit(add_str_res);
end;
function sub_str(sub_str_a: string; sub_str_b: string): string;
var
  sub_str_i: integer;
  sub_str_j: integer;
  sub_str_borrow: int64;
  sub_str_res: string;
  sub_str_da: integer;
  sub_str_db: int64;
  sub_str_diff: integer;
  sub_str_k: int64;
begin
  sub_str_i := Length(sub_str_a) - 1;
  sub_str_j := Length(sub_str_b) - 1;
  sub_str_borrow := 0;
  sub_str_res := '';
  while sub_str_i >= 0 do begin
  sub_str_da := Trunc(digitMap[copy(sub_str_a, sub_str_i+1, (sub_str_i + 1 - (sub_str_i)))]) - sub_str_borrow;
  sub_str_db := 0;
  if sub_str_j >= 0 then begin
  sub_str_db := Trunc(digitMap[copy(sub_str_b, sub_str_j+1, (sub_str_j + 1 - (sub_str_j)))]);
end;
  if sub_str_da < sub_str_db then begin
  sub_str_da := sub_str_da + 10;
  sub_str_borrow := 1;
end else begin
  sub_str_borrow := 0;
end;
  sub_str_diff := sub_str_da - sub_str_db;
  sub_str_res := IntToStr(sub_str_diff) + sub_str_res;
  sub_str_i := sub_str_i - 1;
  sub_str_j := sub_str_j - 1;
end;
  sub_str_k := 0;
  while (sub_str_k < Length(sub_str_res)) and (copy(sub_str_res, sub_str_k+1, (sub_str_k + 1 - (sub_str_k))) = '0') do begin
  sub_str_k := sub_str_k + 1;
end;
  if sub_str_k = Length(sub_str_res) then begin
  exit('0');
end;
  exit(copy(sub_str_res, sub_str_k+1, Length(sub_str_res)));
end;
function mul_digit(mul_digit_a: string; mul_digit_d: int64): string;
var
  mul_digit_i: integer;
  mul_digit_carry: int64;
  mul_digit_res: string;
  mul_digit_prod: integer;
  mul_digit_k: int64;
begin
  if mul_digit_d = 0 then begin
  exit('0');
end;
  mul_digit_i := Length(mul_digit_a) - 1;
  mul_digit_carry := 0;
  mul_digit_res := '';
  while mul_digit_i >= 0 do begin
  mul_digit_prod := (Trunc(digitMap[copy(mul_digit_a, mul_digit_i+1, (mul_digit_i + 1 - (mul_digit_i)))]) * mul_digit_d) + mul_digit_carry;
  mul_digit_res := IntToStr(mul_digit_prod mod 10) + mul_digit_res;
  mul_digit_carry := _floordiv(mul_digit_prod, 10);
  mul_digit_i := mul_digit_i - 1;
end;
  if mul_digit_carry > 0 then begin
  mul_digit_res := IntToStr(mul_digit_carry) + mul_digit_res;
end;
  mul_digit_k := 0;
  while (mul_digit_k < Length(mul_digit_res)) and (copy(mul_digit_res, mul_digit_k+1, (mul_digit_k + 1 - (mul_digit_k))) = '0') do begin
  mul_digit_k := mul_digit_k + 1;
end;
  if mul_digit_k = Length(mul_digit_res) then begin
  exit('0');
end;
  exit(copy(mul_digit_res, mul_digit_k+1, Length(mul_digit_res)));
end;
function mul_str(mul_str_a: string; mul_str_b: string): specialize TFPGMap<string, Variant>;
var
  mul_str_result_: string;
  mul_str_shift: int64;
  mul_str_parts: array of Variant;
  mul_str_i: integer;
  mul_str_d: integer;
  mul_str_part: string;
  mul_str_shifted: string;
  mul_str__: int64;
begin
  mul_str_result_ := '0';
  mul_str_shift := 0;
  mul_str_parts := [];
  mul_str_i := Length(mul_str_b) - 1;
  while mul_str_i >= 0 do begin
  mul_str_d := Trunc(digitMap[copy(mul_str_b, mul_str_i+1, (mul_str_i + 1 - (mul_str_i)))]);
  mul_str_part := mul_digit(mul_str_a, mul_str_d);
  mul_str_parts := concat(mul_str_parts, [Variant(PtrUInt(Map2(mul_str_part, mul_str_shift)))]);
  mul_str_shifted := mul_str_part;
  for mul_str__ := 0 to (mul_str_shift - 1) do begin
  mul_str_shifted := mul_str_shifted + '0';
end;
  mul_str_result_ := add_str(mul_str_result_, mul_str_shifted);
  mul_str_shift := mul_str_shift + 1;
  mul_str_i := mul_str_i - 1;
end;
  exit(Map3(mul_str_parts, mul_str_result_));
end;
function pad_left(pad_left_s: string; pad_left_total: int64): string;
var
  pad_left_r: string;
  pad_left__: int64;
begin
  pad_left_r := '';
  for pad_left__ := 0 to (pad_left_total - Length(pad_left_s) - 1) do begin
  pad_left_r := pad_left_r + ' ';
end;
  exit(pad_left_r + pad_left_s);
end;
procedure main();
var
  main_tStr: string;
  main_t: int64;
  main__: int64;
  main_line: string;
  main_idx: int64;
  main_ch: string;
  main_a: string;
  main_op: string;
  main_b: string;
  main_res: string;
  main_parts: array of Variant;
  main_r: specialize TFPGMap<string, Variant>;
  main_width: integer;
  main_secondLen: integer;
  main_p: Variant;
  main_l: integer;
  main_dash1: int64;
  main_firstPart: integer;
  main_p_19: Variant;
  main_val: integer;
  main_shift: integer;
  main_spaces: integer;
  main_line_23: string;
  main___24: int64;
begin
  main_tStr := _input();
  if main_tStr = '' then begin
  exit();
end;
  main_t := StrToInt(main_tStr);
  for main__ := 0 to (main_t - 1) do begin
  main_line := _input();
  if main_line = '' then begin
  continue;
end;
  main_idx := 0;
  while main_idx < Length(main_line) do begin
  main_ch := copy(main_line, main_idx+1, (main_idx + 1 - (main_idx)));
  if ((main_ch = '+') or (main_ch = '-')) or (main_ch = '*') then begin
  break;
end;
  main_idx := main_idx + 1;
end;
  main_a := copy(main_line, 1, main_idx);
  main_op := copy(main_line, main_idx+1, (main_idx + 1 - (main_idx)));
  main_b := copy(main_line, main_idx + 1+1, Length(main_line));
  main_res := '';
  main_parts := [];
  if main_op = '+' then begin
  main_res := add_str(main_a, main_b);
end else begin
  if main_op = '-' then begin
  main_res := sub_str(main_a, main_b);
end else begin
  main_r := mul_str(main_a, main_b);
  main_res := main_r['res'];
  main_parts := VariantArray(main_r['parts']);
end;
end;
  main_width := Length(main_a);
  main_secondLen := Length(main_b) + 1;
  if main_secondLen > main_width then begin
  main_width := main_secondLen;
end;
  if Length(main_res) > main_width then begin
  main_width := Length(main_res);
end;
  for main_p in main_parts do begin
  main_l := Length(specialize TFPGMap<string, Variant>(pointer(PtrUInt(main_p)))['val']) + Trunc(specialize TFPGMap<string, Variant>(pointer(PtrUInt(main_p)))['shift']);
  if main_l > main_width then begin
  main_width := main_l;
end;
end;
  writeln(pad_left(main_a, main_width));
  writeln(pad_left(main_op + main_b, main_width));
  main_dash1 := 0;
  if main_op = '*' then begin
  if Length(main_parts) > 0 then begin
  main_dash1 := Length(main_b) + 1;
  main_firstPart := specialize TFPGMap<string, Variant>(pointer(PtrUInt(main_parts[0])))['val'];
  if Length(main_firstPart) > main_dash1 then begin
  main_dash1 := Length(main_firstPart);
end;
end else begin
  main_dash1 := Length(main_b) + 1;
  if Length(main_res) > main_dash1 then begin
  main_dash1 := Length(main_res);
end;
end;
end else begin
  main_dash1 := Length(main_b) + 1;
  if Length(main_res) > main_dash1 then begin
  main_dash1 := Length(main_res);
end;
end;
  writeln(pad_left(repeat_('-', main_dash1), main_width));
  if (main_op = '*') and (Length(main_b) > 1) then begin
  for main_p_19 in main_parts do begin
  main_val := specialize TFPGMap<string, Variant>(pointer(PtrUInt(main_p_19)))['val'];
  main_shift := Trunc(specialize TFPGMap<string, Variant>(pointer(PtrUInt(main_p_19)))['shift']);
  main_spaces := (main_width - main_shift) - Length(main_val);
  main_line_23 := '';
  for main___24 := 0 to (main_spaces - 1) do begin
  main_line_23 := main_line_23 + ' ';
end;
  main_line_23 := main_line_23 + main_val;
  writeln(main_line_23);
end;
  writeln(pad_left(repeat_('-', Length(main_res)), main_width));
end;
  writeln(pad_left(main_res, main_width));
  writeln('');
end;
end;
begin
  digitMap := Map1();
  main();
end.

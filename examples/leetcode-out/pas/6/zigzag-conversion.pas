program main;
{$mode objfpc}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type 
  generic TArray<T> = array of T;

function convert(s: string; numRows: integer): string;

var 
  _tmp0: integer;
  ch: integer;
  curr: integer;
  i: integer;
  _result: string;
  row: integer;
  rows: specialize TArray<string>;
  step: integer;
begin
  if ((numRows <= 1) or (numRows >= Length(s))) then
    begin
      result := s;
      exit;
    end;
  rows := specialize TArray<string>([]);
  i := 0;
  while (i < numRows) do
    begin
      rows := Concat(rows, specialize TArray<string>(['']));
      i := i + 1;
    end;
  curr := 0;
  step := 1;
  for _tmp0 := 1 to Length(s) do
    begin
      ch := s[_tmp0];
      rows[curr] := rows[curr] + ch;
      if (curr = 0) then
        begin
          step := 1;
        end
      else if (curr = numRows - 1) then
             begin
               step := -1;
             end;
      curr := curr + step;
    end;
  _result := '';
  for row in rows do
    begin
      _result := _result + row;
    end;
  result := _result;
  exit;
end;

procedure test_example_1;
begin
  if not ((convert('PAYPALISHIRING', 3) = 'PAHNAPLSIIGYIR')) then raise Exception.Create(
                                                                                     'expect failed'
    );
end;

procedure test_example_2;
begin
  if not ((convert('PAYPALISHIRING', 4) = 'PINALSIGYAHRPI')) then raise Exception.Create(
                                                                                     'expect failed'
    );
end;

procedure test_single_row;
begin
  if not ((convert('A', 1) = 'A')) then raise Exception.Create('expect failed');
end;

begin
  test_example_1;
  test_example_2;
  test_single_row;
end.

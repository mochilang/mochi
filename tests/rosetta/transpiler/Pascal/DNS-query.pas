{$mode objfpc}
program Main;
type StrArray = array of string;
type VariantArray = array of Variant;
function _lookup_host(name: string): VariantArray;
begin
  SetLength(Result, 3);
  Result[0] := '2001:2f0:0:8800:226:2dff:fe0b:4311';
  Result[1] := '2001:2f0:0:8800::1:1';
  Result[2] := '210.155.141.200';
end;
function list_to_str(xs: array of string): string;
var i: integer;
begin
  Result := '#(' + sLineBreak;
  for i := 0 to High(xs) do begin
    Result := Result + '  ''' + xs[i] + '''.' + sLineBreak;
  end;
  Result := Result + ')';
end;
var
  res: array of Variant;
  addrs: Variant;
  err: Variant;
begin
  res := _lookup_host('www.kame.net');
  addrs := res[0];
  err := res[1];
  if err = nil then begin
  writeln(addrs);
end else begin
  writeln(err);
end;
end.

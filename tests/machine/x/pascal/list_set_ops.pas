program ListSetOps;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser;

type
  generic TArray<T> = array of T;

  generic function _except<T>(a, b: specialize TArray<T>): specialize TArray<T>;

var i,j: Integer;
  inB: Boolean;
begin
  SetLength(Result, 0);
  for i := 0 to High(a) do
    begin
      inB := False;
      for j := 0 to High(b) do
        if a[i] = b[j] then
          begin
            inB := True;
            Break;
          end;
      if not inB then
        begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)] := a[i];
        end;
    end;
end;

generic function _intersect<T>(a, b: specialize TArray<T>): specialize TArray<T>;

var i,j,k: Integer;
  inB, exists: Boolean;
begin
  SetLength(Result, 0);
  for i := 0 to High(a) do
    begin
      inB := False;
      for j := 0 to High(b) do
        if a[i] = b[j] then
          begin
            inB := True;
            Break;
          end;
      if inB then
        begin
          exists := False;
          for k := 0 to High(Result) do
            if a[i] = Result[k] then
              begin
                exists := True;
                Break;
              end;
          if not exists then
            begin
              SetLength(Result, Length(Result)+1);
              Result[High(Result)] := a[i];
            end;
        end;
    end;
end;

generic procedure _printList<T>(arr: specialize TArray<T>);

var i: Integer;
begin
  for i := 0 to High(arr) do
    begin
      if i > 0 then Write(' ');
      Write(arr[i]);
    end;
  writeln();
end;

generic function _union<T>(a, b: specialize TArray<T>): specialize TArray<T>;

var i,j: Integer;
  exists: Boolean;
begin
  SetLength(Result, 0);
  for i := 0 to High(a) do
    begin
      exists := False;
      for j := 0 to High(Result) do
        if a[i] = Result[j] then
          begin
            exists := True;
            Break;
          end;
      if not exists then
        begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)] := a[i];
        end;
    end;
  for i := 0 to High(b) do
    begin
      exists := False;
      for j := 0 to High(Result) do
        if b[i] = Result[j] then
          begin
            exists := True;
            Break;
          end;
      if not exists then
        begin
          SetLength(Result, Length(Result)+1);
          Result[High(Result)] := b[i];
        end;
    end;
end;

begin
  specialize _printList<integer>(specialize _union<integer>(specialize TArray<integer>([1, 2]),
  specialize TArray<integer>([2, 3])));
  specialize _printList<integer>(specialize _except<integer>(specialize TArray<integer>([1, 2, 3]),
  specialize TArray<integer>([2])));
  specialize _printList<integer>(specialize _intersect<integer>(specialize TArray<integer>([1, 2, 3]
  ), specialize TArray<integer>([2, 4])));
  writeln(Length(Concat(specialize TArray<integer>([1, 2]), specialize TArray<integer>([2, 3]))));
end.

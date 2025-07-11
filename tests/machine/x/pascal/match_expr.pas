program MatchExpr;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

var
  _tmp0: string;
  _tmp1: integer;
  _label: string;
  x: integer;

begin
  x := 2;
  _tmp1 := x;
  if _tmp1 = 1 then
    begin
      _tmp0 := 'one';
      else if _tmp1 = 2 then
             begin
               _tmp0 := 'two';
               else if _tmp1 = 3 then
                      begin
                        _tmp0 := 'three';
                        else
                          begin
                            _tmp0 := 'unknown';
                          end;
                        _label := _tmp0;
                        writeln(_label);
                      end.

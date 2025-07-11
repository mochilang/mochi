program MatchFull;
{$mode objfpc}
{$modeswitch nestedprocvars}

uses SysUtils, fgl, fphttpclient, Classes, Variants, fpjson, jsonparser, fpjsonrtti;

type
  generic TArray<T> = array of T;

function classify(n: integer): string;

var
  _tmp0: string;
  _tmp1: integer;
begin
  _tmp1 := n;
  if _tmp1 = 0 then
    begin
      _tmp0 := 'zero';
      else if _tmp1 = 1 then
             begin
               _tmp0 := 'one';
               else
                 begin
                   _tmp0 := 'many';
                 end;
               result := _tmp0;
               exit;
             end;

      var
        _tmp2: string;
        _tmp3: integer;
        _tmp4: string;
        _tmp5: string;
        _tmp6: string;
        _tmp7: boolean;
        day: string;
        _label: string;
        mood: string;
        ok: boolean;
        status: string;
        x: integer;

      begin
        x := 2;
        _tmp3 := x;
        if _tmp3 = 1 then
          begin
            _tmp2 := 'one';
            else if _tmp3 = 2 then
                   begin
                     _tmp2 := 'two';
                     else if _tmp3 = 3 then
                            begin
                              _tmp2 := 'three';
                              else
                                begin
                                  _tmp2 := 'unknown';
                                end;
                              _label := _tmp2;
                              writeln(_label);
                              day := 'sun';
                              _tmp5 := day;
                              if _tmp5 = 'mon' then
                                begin
                                  _tmp4 := 'tired';
                                  else if _tmp5 = 'fri' then
                                         begin
                                           _tmp4 := 'excited';
                                           else if _tmp5 = 'sun' then
                                                  begin
                                                    _tmp4 := 'relaxed';
                                                    else
                                                      begin
                                                        _tmp4 := 'normal';
                                                      end;
                                                    mood := _tmp4;
                                                    writeln(mood);
                                                    ok := True;
                                                    _tmp7 := ok;
                                                    if _tmp7 = True then
                                                      begin
                                                        _tmp6 := 'confirmed';
                                                        else if _tmp7 = False then
                                                               begin
                                                                 _tmp6 := 'denied';
                                                               end;
                                                        status := _tmp6;
                                                        writeln(status);
                                                        writeln(classify(0));
                                                        writeln(classify(5));
                                                      end.

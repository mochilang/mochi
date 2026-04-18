program Main;
uses SysUtils;
function JoinInts(arr: array of Integer; n: Integer): AnsiString;
var i: Integer; s: AnsiString;
begin s := '['; for i := 0 to n-1 do begin if i>0 then s := s + ','; s := s + IntToStr(arr[i]); end; JoinInts := s + ']'; end;
procedure SortStr(var a: array of AnsiString; n: Integer);
var i,j: Integer; t: AnsiString;
begin for i:=0 to n-1 do for j:=i+1 to n-1 do if a[j] < a[i] then begin t:=a[i]; a[i]:=a[j]; a[j]:=t; end; end;
var t,tc,m,i,j,wlen,total,cnt: Integer; s: AnsiString; words,target,parts: array of AnsiString; ans: array of Integer; same: Boolean;
begin
  if EOF then Halt(0); ReadLn(t);
  for tc:=1 to t do begin
    ReadLn(s); ReadLn(m); SetLength(words,m); for i:=0 to m-1 do ReadLn(words[i]);
    if m=0 then begin Write('[]'); if tc<t then WriteLn; continue; end;
    SetLength(target,m); for i:=0 to m-1 do target[i]:=words[i]; SortStr(target,m); wlen:=Length(words[0]); total:=wlen*m; SetLength(ans, Length(s)+1); cnt:=0;
    for i:=1 to Length(s)-total+1 do begin SetLength(parts,m); for j:=0 to m-1 do parts[j]:=Copy(s, i + j*wlen, wlen); SortStr(parts,m); same:=True; for j:=0 to m-1 do if parts[j] <> target[j] then begin same:=False; break; end; if same then begin ans[cnt]:=i-1; Inc(cnt); end; end;
    Write(JoinInts(ans,cnt)); if tc<t then WriteLn;
  end;
end.

program LongestSubstring;
uses SysUtils;

function Longest(s: String): LongInt;
var last: array[0..255] of LongInt; left, best, right, idx: LongInt; ch: Char;
begin
  for idx := 0 to 255 do last[idx] := -1;
  left := 1; best := 0;
  for right := 1 to Length(s) do begin
    ch := s[right];
    idx := Ord(ch);
    if last[idx] + 1 >= left then left := last[idx] + 2;
    last[idx] := right - 1;
    if right - left + 1 > best then best := right - left + 1;
  end;
  Longest := best;
end;

var t, i: LongInt; s: String;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for i := 1 to t do begin
    ReadLn(s);
    Write(Longest(s));
    if i < t then WriteLn;
  end;
end.

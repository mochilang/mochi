program Main;
uses SysUtils;
type TIntArray = array of LongInt;
function Trap(var h: TIntArray): LongInt;
var left,right,leftMax,rightMax,water,v: LongInt;
begin
  left := 0; right := Length(h) - 1; leftMax := 0; rightMax := 0; water := 0;
  while left <= right do begin
    if leftMax <= rightMax then begin
      v := h[left];
      if v < leftMax then water := water + leftMax - v else leftMax := v;
      left := left + 1;
    end else begin
      v := h[right];
      if v < rightMax then water := water + rightMax - v else rightMax := v;
      right := right - 1;
    end;
  end;
  Trap := water;
end;
var t,tc,n,i: LongInt; arr: TIntArray;
begin
  if EOF then Halt(0); ReadLn(t);
  for tc := 1 to t do begin
    ReadLn(n); SetLength(arr, n);
    for i := 0 to n - 1 do ReadLn(arr[i]);
    if tc > 1 then WriteLn;
    Write(Trap(arr));
  end;
end.

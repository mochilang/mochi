program Main;

function MaxArea(h: array of LongInt): LongInt;
var left, right, best, height, area: LongInt;
begin
  left := 0; right := High(h); best := 0;
  while left < right do
  begin
    if h[left] < h[right] then height := h[left] else height := h[right];
    area := (right - left) * height;
    if area > best then best := area;
    if h[left] < h[right] then Inc(left) else Dec(right);
  end;
  MaxArea := best;
end;

var t, tc, n, i: LongInt; h: array of LongInt;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do begin
    ReadLn(n);
    SetLength(h, n);
    for i := 0 to n - 1 do ReadLn(h[i]);
    Write(MaxArea(h));
    if tc < t then WriteLn;
  end;
end.

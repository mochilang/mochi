program Main;

function MaxProfit(prices: array of LongInt): LongInt;
var i, best: LongInt;
begin
  best := 0;
  for i := 1 to High(prices) do
  begin
    if prices[i] > prices[i - 1] then best := best + (prices[i] - prices[i - 1]);
  end;
  MaxProfit := best;
end;

var t, tc, n, i: LongInt; prices: array of LongInt;
begin
  if EOF then Halt(0);
  ReadLn(t);
  for tc := 1 to t do begin
    ReadLn(n);
    SetLength(prices, n);
    for i := 0 to n - 1 do ReadLn(prices[i]);
    Write(MaxProfit(prices));
    if tc < t then WriteLn;
  end;
end.

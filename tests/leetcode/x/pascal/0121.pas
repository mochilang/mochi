program Main;

function MaxProfit(prices: array of LongInt): LongInt;
var i, minPrice, best, profit: LongInt;
begin
  if Length(prices) = 0 then
  begin
    MaxProfit := 0;
    Exit;
  end;
  minPrice := prices[0];
  best := 0;
  for i := 1 to High(prices) do
  begin
    profit := prices[i] - minPrice;
    if profit > best then best := profit;
    if prices[i] < minPrice then minPrice := prices[i];
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

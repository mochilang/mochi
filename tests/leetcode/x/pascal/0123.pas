program Main;

function MaxProfit(prices: array of LongInt): LongInt;
var i, buy1, sell1, buy2, sell2, p: LongInt;
begin
  buy1 := -1000000000; sell1 := 0; buy2 := -1000000000; sell2 := 0;
  for i := 0 to High(prices) do
  begin
    p := prices[i];
    if -p > buy1 then buy1 := -p;
    if buy1 + p > sell1 then sell1 := buy1 + p;
    if sell1 - p > buy2 then buy2 := sell1 - p;
    if buy2 + p > sell2 then sell2 := buy2 + p;
  end;
  MaxProfit := sell2;
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

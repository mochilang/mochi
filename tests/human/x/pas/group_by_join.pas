program GroupByJoin;

type
  TCustomer = record
    id: integer;
    name: string;
  end;

  TOrder = record
    id: integer;
    customerId: integer;
  end;

  TStat = record
    name: string;
    count: integer;
  end;

var
  customers: array[1..2] of TCustomer = (
    (id:1; name:'Alice'),
    (id:2; name:'Bob')
  );
  orders: array[1..3] of TOrder = (
    (id:100; customerId:1),
    (id:101; customerId:1),
    (id:102; customerId:2)
  );
  stats: array[1..2] of TStat;
  statCount: integer = 0;
  i, idx: integer;
  nameStr: string;
  found: boolean;
begin
  for i := 1 to Length(orders) do
  begin
    nameStr := '';
    for idx := 1 to Length(customers) do
      if customers[idx].id = orders[i].customerId then
        nameStr := customers[idx].name;

    found := False;
    for idx := 1 to statCount do
      if stats[idx].name = nameStr then
      begin
        stats[idx].count := stats[idx].count + 1;
        found := True;
        Break;
      end;
    if not found then
    begin
      Inc(statCount);
      stats[statCount].name := nameStr;
      stats[statCount].count := 1;
    end;
  end;

  Writeln('--- Orders per customer ---');
  for i := 1 to statCount do
    Writeln(stats[i].name, ' orders: ', stats[i].count);
end.

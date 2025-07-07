program GroupByLeftJoin;

type
  TCustomer = record
    id: integer;
    name: string;
  end;

  TOrder = record
    id: integer;
    customerId: integer;
  end;

  TRow = record
    name: string;
    count: integer;
  end;

var
  customers: array[1..3] of TCustomer = (
    (id:1; name:'Alice'),
    (id:2; name:'Bob'),
    (id:3; name:'Charlie')
  );
  orders: array[1..3] of TOrder = (
    (id:100; customerId:1),
    (id:101; customerId:1),
    (id:102; customerId:2)
  );
  stats: array[1..3] of TRow;
  statCount: integer = 0;
  i,j: integer;
  cnt: integer;
begin
  for i := 1 to Length(customers) do
  begin
    cnt := 0;
    for j := 1 to Length(orders) do
      if orders[j].customerId = customers[i].id then
        Inc(cnt);
    Inc(statCount);
    stats[statCount].name := customers[i].name;
    stats[statCount].count := cnt;
  end;

  Writeln('--- Group Left Join ---');
  for i := 1 to statCount do
    Writeln(stats[i].name, ' orders: ', stats[i].count);
end.

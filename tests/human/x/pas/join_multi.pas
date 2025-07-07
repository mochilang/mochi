program JoinMulti;

type
  TCustomer = record
    id: integer;
    name: string;
  end;
  TOrder = record
    id: integer;
    customerId: integer;
  end;
  TItem = record
    orderId: integer;
    sku: string;
  end;
  TRow = record
    name: string;
    sku: string;
  end;

var
  customers: array[1..2] of TCustomer = (
    (id:1; name:'Alice'),
    (id:2; name:'Bob')
  );
  orders: array[1..2] of TOrder = (
    (id:100; customerId:1),
    (id:101; customerId:2)
  );
  items: array[1..2] of TItem = (
    (orderId:100; sku:'a'),
    (orderId:101; sku:'b')
  );
  rows: array[1..2] of TRow;
  rowCount: integer = 0;
  i,j,k: integer;
begin
  for i := 1 to Length(orders) do
    for j := 1 to Length(customers) do
      if orders[i].customerId = customers[j].id then
        for k := 1 to Length(items) do
          if items[k].orderId = orders[i].id then
          begin
            Inc(rowCount);
            rows[rowCount].name := customers[j].name;
            rows[rowCount].sku := items[k].sku;
          end;

  Writeln('--- Multi Join ---');
  for i := 1 to rowCount do
    Writeln(rows[i].name, ' bought item ', rows[i].sku);
end.

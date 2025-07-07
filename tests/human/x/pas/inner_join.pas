program InnerJoin;

type
  TCustomer = record
    id: integer;
    name: string;
  end;

  TOrder = record
    id: integer;
    customerId: integer;
    total: integer;
  end;

  TRow = record
    orderId: integer;
    customerName: string;
    total: integer;
  end;

var
  customers: array[1..3] of TCustomer = (
    (id:1; name:'Alice'),
    (id:2; name:'Bob'),
    (id:3; name:'Charlie')
  );
  orders: array[1..4] of TOrder = (
    (id:100; customerId:1; total:250),
    (id:101; customerId:2; total:125),
    (id:102; customerId:1; total:300),
    (id:103; customerId:4; total:80)
  );
  rows: array[1..4] of TRow;
  rowCount: integer = 0;
  i,j: integer;
begin
  for i := 1 to Length(orders) do
    for j := 1 to Length(customers) do
      if orders[i].customerId = customers[j].id then
      begin
        Inc(rowCount);
        rows[rowCount].orderId := orders[i].id;
        rows[rowCount].customerName := customers[j].name;
        rows[rowCount].total := orders[i].total;
      end;

  Writeln('--- Orders with customer info ---');
  for i := 1 to rowCount do
    Writeln('Order ', rows[i].orderId, ' by ', rows[i].customerName,
            ' - $', rows[i].total);
end.

program LeftJoin;

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
    customer: string;
    total: integer;
  end;

var
  customers: array[1..2] of TCustomer = (
    (id:1; name:'Alice'),
    (id:2; name:'Bob')
  );
  orders: array[1..2] of TOrder = (
    (id:100; customerId:1; total:250),
    (id:101; customerId:3; total:80)
  );
  rows: array[1..2] of TRow;
  rowCount: integer = 0;
  i,j: integer;
  found: boolean;
begin
  for i := 1 to Length(orders) do
  begin
    found := False;
    for j := 1 to Length(customers) do
      if customers[j].id = orders[i].customerId then
      begin
        found := True;
        break;
      end;
    Inc(rowCount);
    rows[rowCount].orderId := orders[i].id;
    if found then
      rows[rowCount].customer := customers[j].name
    else
      rows[rowCount].customer := 'nil';
    rows[rowCount].total := orders[i].total;
  end;

  Writeln('--- Left Join ---');
  for i := 1 to rowCount do
    Writeln('Order ', rows[i].orderId, ' customer ', rows[i].customer,
            ' total ', rows[i].total);
end.

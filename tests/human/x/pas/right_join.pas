program RightJoin;

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
    customerName: string;
    orderId: integer;
    total: integer;
  end;

var
  customers: array[1..4] of TCustomer = (
    (id:1; name:'Alice'),
    (id:2; name:'Bob'),
    (id:3; name:'Charlie'),
    (id:4; name:'Diana')
  );
  orders: array[1..3] of TOrder = (
    (id:100; customerId:1; total:250),
    (id:101; customerId:2; total:125),
    (id:102; customerId:1; total:300)
  );
  rows: array[1..4] of TRow;
  rowCount: integer = 0;
  i,j: integer;
  matched: boolean;
  found: boolean;
  custName: string;
begin
  for i := 1 to Length(customers) do
  begin
    matched := False;
    for j := 1 to Length(orders) do
      if orders[j].customerId = customers[i].id then
      begin
        Inc(rowCount);
        rows[rowCount].customerName := customers[i].name;
        rows[rowCount].orderId := orders[j].id;
        rows[rowCount].total := orders[j].total;
        matched := True;
      end;
    if not matched then
    begin
      Inc(rowCount);
      rows[rowCount].customerName := customers[i].name;
      rows[rowCount].orderId := 0;
      rows[rowCount].total := 0;
    end;
  end;

  Writeln('--- Right Join using syntax ---');
  for i := 1 to rowCount do
  begin
    if rows[i].orderId <> 0 then
      Writeln('Customer ', rows[i].customerName, ' has order ', rows[i].orderId,
              ' - $', rows[i].total)
    else
      Writeln('Customer ', rows[i].customerName, ' has no orders');
  end;
end.

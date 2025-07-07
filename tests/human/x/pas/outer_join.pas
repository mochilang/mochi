program OuterJoin;

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
  customers: array[1..4] of TCustomer = (
    (id:1; name:'Alice'),
    (id:2; name:'Bob'),
    (id:3; name:'Charlie'),
    (id:4; name:'Diana')
  );
  orders: array[1..4] of TOrder = (
    (id:100; customerId:1; total:250),
    (id:101; customerId:2; total:125),
    (id:102; customerId:1; total:300),
    (id:103; customerId:5; total:80)
  );
  rows: array[1..8] of TRow;
  rowCount: integer = 0;
  i,j: integer;
  matched: boolean;
  found: boolean;
begin
  for i := 1 to Length(orders) do
  begin
    found := False;
    for j := 1 to Length(customers) do
      if customers[j].id = orders[i].customerId then
      begin
        Inc(rowCount);
        rows[rowCount].orderId := orders[i].id;
        rows[rowCount].customerName := customers[j].name;
        rows[rowCount].total := orders[i].total;
        found := True;
        matched := True;
      end;
    if not found then
    begin
      Inc(rowCount);
      rows[rowCount].orderId := orders[i].id;
      rows[rowCount].customerName := 'Unknown';
      rows[rowCount].total := orders[i].total;
    end;
  end;

  { Customers with no orders }
  for i := 1 to Length(customers) do
  begin
    matched := False;
    for j := 1 to Length(orders) do
      if orders[j].customerId = customers[i].id then
        matched := True;
    if not matched then
    begin
      Inc(rowCount);
      rows[rowCount].orderId := 0;
      rows[rowCount].customerName := customers[i].name;
      rows[rowCount].total := 0;
    end;
  end;

  Writeln('--- Outer Join using syntax ---');
  for i := 1 to rowCount do
  begin
    if rows[i].orderId <> 0 then
      Writeln('Order ', rows[i].orderId, ' by ', rows[i].customerName,
              ' - $', rows[i].total)
    else
      Writeln('Customer ', rows[i].customerName, ' has no orders');
  end;
end.

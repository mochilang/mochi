program LeftJoinMulti;

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
    orderId: integer;
    name: string;
    item: string;
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
  items: array[1..1] of TItem = (
    (orderId:100; sku:'a')
  );
  rows: array[1..2] of TRow;
  rowCount: integer = 0;
  i,j,k: integer;
  found: boolean;
  itemName: string;
begin
  for i := 1 to Length(orders) do
    for j := 1 to Length(customers) do
      if customers[j].id = orders[i].customerId then
      begin
        itemName := 'nil';
        found := False;
        for k := 1 to Length(items) do
          if items[k].orderId = orders[i].id then
          begin
            itemName := items[k].sku;
            found := True;
            break;
          end;
        Inc(rowCount);
        rows[rowCount].orderId := orders[i].id;
        rows[rowCount].name := customers[j].name;
        if found then
          rows[rowCount].item := itemName
        else
          rows[rowCount].item := 'nil';
      end;

  Writeln('--- Left Join Multi ---');
  for i := 1 to rowCount do
    Writeln(rows[i].orderId, ' ', rows[i].name, ' ', rows[i].item);
end.

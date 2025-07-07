program CrossJoin;

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

var
  customers: array[1..3] of TCustomer = (
    (id:1; name:'Alice'),
    (id:2; name:'Bob'),
    (id:3; name:'Charlie')
  );
  orders: array[1..3] of TOrder = (
    (id:100; customerId:1; total:250),
    (id:101; customerId:2; total:125),
    (id:102; customerId:1; total:300)
  );
  i,j: integer;
begin
  Writeln('--- Cross Join: All order-customer pairs ---');
  for i := 1 to Length(orders) do
    for j := 1 to Length(customers) do
      Writeln('Order ', orders[i].id,
              ' (customerId: ', orders[i].customerId,
              ', total: $', orders[i].total,
              ') paired with ', customers[j].name);
end.

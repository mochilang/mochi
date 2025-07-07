program GroupByMultiJoin;

type
  TNation = record
    id: integer;
    name: string;
  end;

  TSupplier = record
    id: integer;
    nation: integer;
  end;

  TPartSupp = record
    part: integer;
    supplier: integer;
    cost: real;
    qty: integer;
  end;

  TRow = record
    part: integer;
    total: real;
  end;

var
  nations: array[1..2] of TNation = (
    (id:1; name:'A'),
    (id:2; name:'B')
  );
  suppliers: array[1..2] of TSupplier = (
    (id:1; nation:1),
    (id:2; nation:2)
  );
  partsupp: array[1..3] of TPartSupp = (
    (part:100; supplier:1; cost:10.0; qty:2),
    (part:100; supplier:2; cost:20.0; qty:1),
    (part:200; supplier:1; cost:5.0; qty:3)
  );
  rows: array[1..3] of TRow;
  rowCount: integer = 0;
  i,j,k: integer;
  value: real;
  found: boolean;
  idx: integer;
begin
  for i := 1 to Length(partsupp) do
    for j := 1 to Length(suppliers) do
      if partsupp[i].supplier = suppliers[j].id then
        for k := 1 to Length(nations) do
          if suppliers[j].nation = nations[k].id then
            if nations[k].name = 'A' then
            begin
              value := partsupp[i].cost * partsupp[i].qty;
              found := False;
              for idx := 1 to rowCount do
                if rows[idx].part = partsupp[i].part then
                begin
                  rows[idx].total := rows[idx].total + value;
                  found := True;
                  Break;
                end;
              if not found then
              begin
                Inc(rowCount);
                rows[rowCount].part := partsupp[i].part;
                rows[rowCount].total := value;
              end;
            end;

  for i := 1 to rowCount do
    Writeln('part=', rows[i].part, ' total=', rows[i].total:0:1);
end.

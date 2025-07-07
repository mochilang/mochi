program DatasetSortTakeLimit;

type
  TProduct = record
    name: string;
    price: integer;
  end;

var
  products: array[1..7] of TProduct = (
    (name:'Laptop'; price:1500),
    (name:'Smartphone'; price:900),
    (name:'Tablet'; price:600),
    (name:'Monitor'; price:300),
    (name:'Keyboard'; price:100),
    (name:'Mouse'; price:50),
    (name:'Headphones'; price:200)
  );
  i,j: integer;
  temp: TProduct;
begin
  { sort by price descending }
  for i := 1 to High(products)-1 do
    for j := i+1 to High(products) do
      if products[i].price < products[j].price then
      begin
        temp := products[i];
        products[i] := products[j];
        products[j] := temp;
      end;

  Writeln('--- Top products (excluding most expensive) ---');
  for i := 2 to 4 do
    Writeln(products[i].name, ' costs $', products[i].price);
end.

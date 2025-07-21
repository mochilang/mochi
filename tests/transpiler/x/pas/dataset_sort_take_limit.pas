{$mode objfpc}
program Main;
type Anon6 = record
  name: string;
  price: integer;
end;
var
  products: array of Anon6;
  expensive: array of Anon6;
  p: Anon6;
  item: integer;
begin
  products := [(name: 'Laptop'; price: 1500), (name: 'Smartphone'; price: 900), (name: 'Tablet'; price: 600), (name: 'Monitor'; price: 300), (name: 'Keyboard'; price: 100), (name: 'Mouse'; price: 50), (name: 'Headphones'; price: 200)];
  expensive := [];
  for p in products do begin
  expensive := concat(expensive, [p]);
end;
  writeln('--- Top products (excluding most expensive) ---');
  for item in expensive do begin
  writeln(item.name, 'costs $', item.price);
end;
end.

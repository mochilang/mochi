{$mode objfpc}
program Main;
var
  main_list: array of integer;
  main_a: integer;
  main_d: integer;
  main_e: integer;
  main_i: integer;
procedure main();
begin
  main_list := [];
  main_a := 1;
  main_d := 2;
  main_e := 3;
  main_i := 4;
  main_list := concat(main_list, [main_a]);
  main_list := concat(main_list, [main_d]);
  main_list := concat(main_list, [main_e]);
  main_list := concat(main_list, [main_i]);
  main_i := Length(main_list);
end;
begin
  main();
end.

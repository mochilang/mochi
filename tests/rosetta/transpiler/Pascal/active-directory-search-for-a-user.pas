{$mode objfpc}
program Main;
uses fgl;
type StrArray = array of string;
type Anon1 = record
  Base: string;
  Host: string;
  Port: integer;
  GroupFilter: string;
end;
type Anon2 = record
  username: StrArray;
  john: StrArray;
end;
var
  main_client: Anon1;
  main_directory: Anon2;
  main_groups: StrArray;
  main_out: string;
  main_i: integer;
function makeAnon2(username: StrArray; john: StrArray): Anon2; forward;
function makeAnon1(Base: string; Host: string; Port: integer; GroupFilter: string): Anon1; forward;
function search_user(directory: specialize TFPGMap<string, StrArray>; username: string): StrArray; forward;
procedure main(); forward;
function makeAnon2(username: StrArray; john: StrArray): Anon2;
begin
  Result.username := username;
  Result.john := john;
end;
function makeAnon1(Base: string; Host: string; Port: integer; GroupFilter: string): Anon1;
begin
  Result.Base := Base;
  Result.Host := Host;
  Result.Port := Port;
  Result.GroupFilter := GroupFilter;
end;
function search_user(directory: specialize TFPGMap<string, StrArray>; username: string): StrArray;
begin
  exit(directory[username]);
end;
procedure main();
begin
  main_client := makeAnon1('dc=example,dc=com', 'ldap.example.com', 389, '(memberUid=%s)');
  main_directory := makeAnon2(['admins', 'users'], ['users']);
  main_groups := search_user(main_directory, 'username');
  if Length(main_groups) > 0 then begin
  main_out := 'Groups: [';
  main_i := 0;
  while main_i < Length(main_groups) do begin
  main_out := ((main_out + '"') + main_groups[main_i]) + '"';
  if main_i < (Length(main_groups) - 1) then begin
  main_out := main_out + ', ';
end;
  main_i := main_i + 1;
end;
  main_out := main_out + ']';
  writeln(main_out);
end else begin
  writeln('User not found');
end;
end;
begin
  main();
end.

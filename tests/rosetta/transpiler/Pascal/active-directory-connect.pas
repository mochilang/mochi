{$mode objfpc}
program Main;
type StrArray = array of string;
type LDAPClient = record
  Base: string;
  Host: string;
  Port: integer;
  UseSSL: boolean;
  BindDN: string;
  BindPassword: string;
  UserFilter: string;
  GroupFilter: string;
  Attributes: array of string;
end;
var
  main_client: LDAPClient;
function makeLDAPClient(Base: string; Host: string; Port: integer; UseSSL: boolean; BindDN: string; BindPassword: string; UserFilter: string; GroupFilter: string; Attributes: StrArray): LDAPClient; forward;
function connect(client: LDAPClient): boolean; forward;
procedure main(); forward;
function makeLDAPClient(Base: string; Host: string; Port: integer; UseSSL: boolean; BindDN: string; BindPassword: string; UserFilter: string; GroupFilter: string; Attributes: StrArray): LDAPClient;
begin
  Result.Base := Base;
  Result.Host := Host;
  Result.Port := Port;
  Result.UseSSL := UseSSL;
  Result.BindDN := BindDN;
  Result.BindPassword := BindPassword;
  Result.UserFilter := UserFilter;
  Result.GroupFilter := GroupFilter;
  Result.Attributes := Attributes;
end;
function connect(client: LDAPClient): boolean;
begin
  exit((client.Host <> '') and (client.Port > 0));
end;
procedure main();
begin
  main_client.Base := 'dc=example,dc=com';
  main_client.Host := 'ldap.example.com';
  main_client.Port := 389;
  main_client.UseSSL := false;
  main_client.BindDN := 'uid=readonlyuser,ou=People,dc=example,dc=com';
  main_client.BindPassword := 'readonlypassword';
  main_client.UserFilter := '(uid=%s)';
  main_client.GroupFilter := '(memberUid=%s)';
  main_client.Attributes := ['givenName', 'sn', 'mail', 'uid'];
  if connect(main_client) then begin
  writeln('Connected to ' + main_client.Host);
end else begin
  writeln('Failed to connect');
end;
end;
begin
  main();
end.

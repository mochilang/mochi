// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:05Z
#include <iostream>
#include <string>
#include <vector>

struct LDAPClient {
  std::string Base;
  std::string Host;
  int Port;
  bool UseSSL;
  std::string BindDN;
  std::string BindPassword;
  std::string UserFilter;
  std::string GroupFilter;
  auto Attributes;
};

bool connect(LDAPClient client) {
  return ((client.Host != std::string("")) && (client.Port > 0));
}

auto main() {
  auto client = LDAPClient{
      std::string("dc=example,dc=com"),
      std::string("ldap.example.com"),
      389,
      false,
      std::string("uid=readonlyuser,ou=People,dc=example,dc=com"),
      std::string("readonlypassword"),
      std::string("(uid=%s)"),
      std::string("(memberUid=%s)"),
      std::vector<std::string>{std::string("givenName"), std::string("sn"),
                               std::string("mail"), std::string("uid")}};
  if (connect(client)) {
    std::cout << (std::string("Connected to ") + client.Host) << std::endl;
  } else {
    std::cout << std::string("Failed to connect") << std::endl;
  }
}

int main() {
  main();
  return 0;
}

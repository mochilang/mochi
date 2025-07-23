<?php
ini_set('memory_limit','-1');
function connect($client) {
  global $main;
  return $client["Host"] != "" && $client["Port"] > 0;
}
function main() {
  global $connect;
  $client = ["Base" => "dc=example,dc=com", "Host" => "ldap.example.com", "Port" => 389, "UseSSL" => false, "BindDN" => "uid=readonlyuser,ou=People,dc=example,dc=com", "BindPassword" => "readonlypassword", "UserFilter" => "(uid=%s)", "GroupFilter" => "(memberUid=%s)", "Attributes" => ["givenName", "sn", "mail", "uid"]];
  if (connect($client)) {
  echo "Connected to " . $client["Host"], PHP_EOL;
} else {
  echo "Failed to connect", PHP_EOL;
}
}
main();

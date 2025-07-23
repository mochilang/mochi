<?php
ini_set('memory_limit','-1');
function search_user($directory, $username) {
  global $main;
  return $directory[$username];
}
function main() {
  global $search_user;
  $client = ["Base" => "dc=example,dc=com", "Host" => "ldap.example.com", "Port" => 389, "GroupFilter" => "(memberUid=%s)"];
  $directory = ["username" => ["admins", "users"], "john" => ["users"]];
  $groups = search_user($directory, "username");
  if (count($groups) > 0) {
  $out = "Groups: [";
  $i = 0;
  while ($i < count($groups)) {
  $out = $out . "\"" . $groups[$i] . "\"";
  if ($i < count($groups) - 1) {
  $out = $out . ", ";
}
  $i = $i + 1;
};
  $out = $out . "]";
  echo $out, PHP_EOL;
} else {
  echo "User not found", PHP_EOL;
}
}
main();

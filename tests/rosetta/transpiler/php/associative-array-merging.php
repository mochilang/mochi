<?php
function merge($base, $update) {
  global $main;
  $result = [];
  foreach (array_keys($base) as $k) {
  $result[$k] = $base[$k];
};
  foreach (array_keys($update) as $k) {
  $result[$k] = $update[$k];
};
  return $result;
}
function main() {
  global $merge;
  $base = ["name" => "Rocket Skates", "price" => 12.75, "color" => "yellow"];
  $update = ["price" => 15.25, "color" => "red", "year" => 1974];
  $result = merge($base, $update);
  echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode($result, 1344)))))), PHP_EOL;
}
main();

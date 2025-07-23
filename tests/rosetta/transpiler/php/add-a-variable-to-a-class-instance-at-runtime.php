<?php
function main() {
  $ss = ["runtimeFields" => []];
  echo "Create two fields at runtime: \n", PHP_EOL;
  $i = 1;
  while ($i <= 2) {
  echo "  Field #" . json_encode($i, 1344) . ":\n", PHP_EOL;
  echo "       Enter name  : ", PHP_EOL;
  $name = trim(fgets(STDIN));
  echo "       Enter value : ", PHP_EOL;
  $value = trim(fgets(STDIN));
  $fields = $ss["runtimeFields"];
  $fields[$name] = $value;
  $ss["runtimeFields"] = $fields;
  echo "\n", PHP_EOL;
  $i = $i + 1;
};
  while (true) {
  echo "Which field do you want to inspect ? ", PHP_EOL;
  $name = trim(fgets(STDIN));
  if (array_key_exists($name, $ss["runtimeFields"])) {
  $value = $ss["runtimeFields"][$name];
  echo "Its value is '" . $value . "'", PHP_EOL;
  return;
} else {
  echo "There is no field of that name, try again\n", PHP_EOL;
}
};
}
main();

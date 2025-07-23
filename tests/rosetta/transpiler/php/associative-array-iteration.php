<?php
function main() {
  $m = ["hello" => 13, "world" => 31, "!" => 71];
  foreach (array_keys($m) as $k) {
  echo "key = " . $k . ", value = " . json_encode($m[$k], 1344), PHP_EOL;
};
  foreach (array_keys($m) as $k) {
  echo "key = " . $k, PHP_EOL;
};
  foreach (array_keys($m) as $k) {
  echo "value = " . json_encode($m[$k], 1344), PHP_EOL;
};
}
main();

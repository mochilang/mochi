<?php
$nums = [1, 2];
$letters = ["A", "B"];
$bools = [true, false];
$combos = [];
foreach ($nums as $n) {
  foreach ($letters as $l) {
    foreach ($bools as $b) {
      $combos[] = ["n" => $n, "l" => $l, "b" => $b];
    }
  }
}

echo "--- Cross Join of three lists ---", PHP_EOL;
foreach ($combos as $c) {
  echo (is_float($c["n"]) ? json_encode($c["n"], 1344) : $c["n"]) . " " . (is_float($c["l"]) ? json_encode($c["l"], 1344) : $c["l"]) . " " . (is_float($c["b"]) ? json_encode($c["b"], 1344) : $c["b"]), PHP_EOL;
}

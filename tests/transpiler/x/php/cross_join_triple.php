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

echo rtrim("--- Cross Join of three lists ---"), PHP_EOL;
foreach ($combos as $c) {
  echo rtrim($c["n"] . " " . $c["l"] . " " . $c["b"]), PHP_EOL;
}
?>

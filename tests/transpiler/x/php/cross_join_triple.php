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
  echo rtrim((is_float($c["n"]) ? sprintf("%.15f", $c["n"]) : $c["n"]) . " " . (is_float($c["l"]) ? sprintf("%.15f", $c["l"]) : $c["l"]) . " " . (is_float($c["b"]) ? sprintf("%.15f", $c["b"]) : $c["b"])), PHP_EOL;
}
?>

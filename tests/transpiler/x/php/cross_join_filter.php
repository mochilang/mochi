<?php
$nums = [1, 2, 3];
$letters = ["A", "B"];
$pairs = [];
foreach ($nums as $n) {
  foreach ($letters as $l) {
    if ($n % 2 == 0) {
      $pairs[] = ["n" => $n, "l" => $l];
    }
  }
}

echo rtrim("--- Even pairs ---"), PHP_EOL;
foreach ($pairs as $p) {
  echo rtrim((is_float($p["n"]) ? sprintf("%.15f", $p["n"]) : $p["n"]) . " " . (is_float($p["l"]) ? sprintf("%.15f", $p["l"]) : $p["l"])), PHP_EOL;
}
?>

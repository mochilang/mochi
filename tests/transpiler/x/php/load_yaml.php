<?php
$people = (function() { $lines = file("/workspace/mochi/tests/interpreter/valid/people.yaml", FILE_IGNORE_NEW_LINES | FILE_SKIP_EMPTY_LINES); $rows = []; $curr = []; foreach ($lines as $line) { $line = trim($line); if (str_starts_with($line, '-')) { if ($curr) $rows[] = $curr; $curr = []; $line = trim(substr($line, 1)); if ($line !== '') { [$k,$v] = array_map('trim', explode(':', $line, 2)); $curr[$k] = is_numeric($v) ? (int)$v : $v; } } else { [$k,$v] = array_map('trim', explode(':', $line, 2)); $curr[$k] = is_numeric($v) ? (int)$v : $v; } } if ($curr) $rows[] = $curr; return $rows; })();
$adults = [];
foreach ($people as $p) {
  if ($p["age"] >= 18) {
    $adults[] = ["name" => $p["name"], "email" => $p["email"]];
  }
}

foreach ($adults as $a) {
  echo (is_float($a["name"]) ? json_encode($a["name"], 1344) : $a["name"]) . " " . (is_float($a["email"]) ? json_encode($a["email"], 1344) : $a["email"]), PHP_EOL;
}

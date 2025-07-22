<?php
$nums = [1, 2, 3];
$result = (function() use ($nums) { $s = 0; foreach ((function() use ($nums) {
  $result = [];
  foreach ($nums as $n) {
    if ($n > 1) {
      $result[] = $n;
    }
  }
  return $result;
})() as $_v) { $s += $_v; } return $s; })();
echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode($result, 1344)))))), PHP_EOL;

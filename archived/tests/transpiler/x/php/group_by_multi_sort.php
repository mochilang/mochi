<?php
$items = [["a" => "x", "b" => 1, "val" => 2], ["a" => "x", "b" => 2, "val" => 3], ["a" => "y", "b" => 1, "val" => 4], ["a" => "y", "b" => 2, "val" => 1]];
$grouped = (function() use ($items) {
  $groups = [];
  foreach ($items as $i) {
    $key = ["a" => $i["a"], "b" => $i["b"]];
    $k = json_encode($key);
    if (!array_key_exists($k, $groups)) {
      $groups[$k] = ['key' => $key, 'items' => []];
    }
    $groups[$k]['items'][] = $i;
  }
  $result = [];
  foreach ($groups as $g) {
      $result[] = [-array_sum((function() use ($i, $g) {
  $result = [];
  foreach ($g["items"] as $x) {
    $result[] = $x["val"];
  }
  return $result;
})()), ["a" => $g["key"]["a"], "b" => $g["key"]["b"], "total" => array_sum((function() use ($i, $g) {
  $result = [];
  foreach ($g["items"] as $x) {
    $result[] = $x["val"];
  }
  return $result;
})())]];
  }
  usort($result, function($a, $b) { return $a[0] <=> $b[0]; });
  $result = array_map(fn($r) => $r[1], $result);
  return $result;
})();
echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode($grouped, 1344)))))), PHP_EOL;

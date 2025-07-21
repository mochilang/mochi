<?php
$items = [["cat" => "a", "val" => 10, "flag" => true], ["cat" => "a", "val" => 5, "flag" => false], ["cat" => "b", "val" => 20, "flag" => true]];
$result = (function() use ($items) {
  $groups = [];
  foreach ($items as $i) {
    $key = $i["cat"];
    $k = json_encode($key);
    if (!array_key_exists($k, $groups)) {
      $groups[$k] = ['key' => $key, 'items' => []];
    }
    $groups[$k]['items'][] = $i;
  }
  ksort($groups);
  $result = [];
  foreach ($groups as $g) {
      $result[] = ["cat" => $g["key"], "share" => array_sum((function() use ($i, $g) {
  $result = [];
  foreach ($g["items"] as $x) {
    $result[] = ($x["flag"] ? $x["val"] : 0);
  }
  return $result;
})()) / array_sum((function() use ($i, $g) {
  $result = [];
  foreach ($g["items"] as $x) {
    $result[] = $x["val"];
  }
  return $result;
})())];
  }
  return $result;
})();
echo str_replace("false", "False", str_replace("true", "True", str_replace("\"", "'", str_replace(":", ": ", str_replace(",", ", ", json_encode($result, 1344)))))), PHP_EOL;

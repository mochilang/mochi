<?php
$items = [["cat" => "a", "val" => 3], ["cat" => "a", "val" => 1], ["cat" => "b", "val" => 5], ["cat" => "b", "val" => 2]];
$grouped = (function() use ($items) {
  $groups = [];
  foreach ($items as $i) {
    $key = $i["cat"];
    if (!array_key_exists($key, $groups)) {
      $groups[$key] = ['key' => $key, 'items' => []];
    }
    $groups[$key]['items'][] = $i;
  }
  $result = [];
  foreach ($groups as $g) {
      $result[] = [-array_sum((function() use ($i, $g) {
  $result = [];
  foreach ($g["items"] as $x) {
    $result[] = $x["val"];
  }
  return $result;
})()), ["cat" => $g["key"], "total" => array_sum((function() use ($i, $g) {
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
echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode($grouped, 320)))), PHP_EOL;
?>

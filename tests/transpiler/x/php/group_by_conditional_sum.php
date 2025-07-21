<?php
$items = [["cat" => "a", "val" => 10, "flag" => true], ["cat" => "a", "val" => 5, "flag" => false], ["cat" => "b", "val" => 20, "flag" => true]];
$result = (function() use ($items) {
  $groups = [];
  foreach ($items as $i) {
    $key = $i["cat"];
    if (!array_key_exists($key, $groups)) {
      $groups[$key] = ['key' => $key, 'items' => []];
    }
    $groups[$key]['items'][] = $i;
  }
  ksort($groups);
  $result = [];
  foreach ($groups as $g) {
      $result[] = ["cat" => $g["key"], "share" => intdiv(array_sum((function() use ($i, $g) {
  $result = [];
  foreach ($g["items"] as $x) {
    $result[] = ($x["flag"] ? $x["val"] : 0);
  }
  return $result;
})()), array_sum((function() use ($i, $g) {
  $result = [];
  foreach ($g["items"] as $x) {
    $result[] = $x["val"];
  }
  return $result;
})()))];
  }
  return $result;
})();
echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode($result, 320)))), PHP_EOL;
?>

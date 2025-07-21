<?php
$data = [["tag" => "a", "val" => 1], ["tag" => "a", "val" => 2], ["tag" => "b", "val" => 3]];
$groups = (function() use ($data) {
  $groups = [];
  foreach ($data as $d) {
    $key = $d["tag"];
    if (!array_key_exists($key, $groups)) {
      $groups[$key] = ['key' => $key, 'items' => []];
    }
    $groups[$key]['items'][] = $d;
  }
  $result = [];
  foreach ($groups as $g) {
      $result[] = $g;
  }
  return $result;
})();
$tmp = [];
foreach ($groups as $g) {
  $total = 0;
  foreach ($g["items"] as $x) {
  $total = $total + $x["val"];
};
  $tmp = array_merge($tmp, [["tag" => $g["key"], "total" => $total]]);
}
$result = [];
foreach ($tmp as $r) {
  $result[] = $r;
}

echo rtrim(str_replace(":", ": ", str_replace(",", ", ", json_encode($result, 320)))), PHP_EOL;
?>

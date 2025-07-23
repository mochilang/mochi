<?php
function poolPut(&$p, $x) {
  global $poolGet, $clearPool, $main;
  return array_merge($p, [$x]);
}
function poolGet(&$p) {
  global $poolPut, $clearPool, $main;
  if (count($p) == 0) {
  echo "pool empty", PHP_EOL;
  return ["pool" => $p, "val" => 0];
}
  $idx = count($p) - 1;
  $v = $p[$idx];
  $p = array_slice($p, 0, $idx - 0);
  return ["pool" => $p, "val" => $v];
}
function clearPool(&$p) {
  global $poolPut, $poolGet, $main;
  return [];
}
function main() {
  global $poolPut, $poolGet, $clearPool;
  $pool = [];
  $i = 1;
  $j = 2;
  echo json_encode($i + $j, 1344), PHP_EOL;
  $pool = poolPut($pool, $i);
  $pool = poolPut($pool, $j);
  $i = 0;
  $j = 0;
  $res1 = poolGet($pool);
  $pool = $res1["pool"];
  $i = intval($res1["val"]);
  $res2 = poolGet($pool);
  $pool = $res2["pool"];
  $j = intval($res2["val"]);
  $i = 4;
  $j = 5;
  echo json_encode($i + $j, 1344), PHP_EOL;
  $pool = poolPut($pool, $i);
  $pool = poolPut($pool, $j);
  $i = 0;
  $j = 0;
  $pool = clearPool($pool);
  $res3 = poolGet($pool);
  $pool = $res3["pool"];
  $i = intval($res3["val"]);
  $res4 = poolGet($pool);
  $pool = $res4["pool"];
  $j = intval($res4["val"]);
  $i = 7;
  $j = 8;
  echo json_encode($i + $j, 1344), PHP_EOL;
}
main();

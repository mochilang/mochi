<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function create_queue($capacity) {
  $arr = [];
  $i = 0;
  while ($i < $capacity) {
  $arr = _append($arr, 0);
  $i = $i + 1;
};
  return ['data' => $arr, 'front' => 0, 'rear' => 0, 'size' => 0, 'capacity' => $capacity];
}
function length($q) {
  return $q['size'];
}
function is_empty($q) {
  return $q['size'] == 0;
}
function front($q) {
  if (is_empty($q)) {
  return 0;
}
  return $q['data'][$q['front']];
}
function enqueue(&$q, $value) {
  if ($q['size'] >= $q['capacity']) {
  $panic('QUEUE IS FULL');
}
  $arr = $q['data'];
  $arr[$q['rear']] = $value;
  $q['data'] = $arr;
  $q['rear'] = fmod(($q['rear'] + 1), $q['capacity']);
  $q['size'] = $q['size'] + 1;
  return $q;
}
function dequeue(&$q) {
  if ($q['size'] == 0) {
  $panic('UNDERFLOW');
}
  $value = $q['data'][$q['front']];
  $arr2 = $q['data'];
  $arr2[$q['front']] = 0;
  $q['data'] = $arr2;
  $q['front'] = fmod(($q['front'] + 1), $q['capacity']);
  $q['size'] = $q['size'] - 1;
  return ['queue' => $q, 'value' => $value];
}
function main() {
  $q = create_queue(5);
  echo rtrim(json_encode(is_empty($q), 1344)), PHP_EOL;
  $q = enqueue($q, 10);
  echo rtrim(json_encode(is_empty($q), 1344)), PHP_EOL;
  $q = enqueue($q, 20);
  $q = enqueue($q, 30);
  echo rtrim(json_encode(front($q), 1344)), PHP_EOL;
  $r = dequeue($q);
  $q = $r['queue'];
  echo rtrim(json_encode($r['value'], 1344)), PHP_EOL;
  echo rtrim(json_encode(front($q), 1344)), PHP_EOL;
  echo rtrim(json_encode(length($q), 1344)), PHP_EOL;
}
main();

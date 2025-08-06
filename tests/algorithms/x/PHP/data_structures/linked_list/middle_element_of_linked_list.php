<?php
ini_set('memory_limit', '-1');
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function empty_list() {
  return ['data' => []];
}
function push($lst, $value) {
  $res = [$value];
  $i = 0;
  while ($i < _len($lst['data'])) {
  $res = _append($res, $lst['data'][$i]);
  $i = $i + 1;
};
  return ['data' => $res];
}
function middle_element($lst) {
  $n = _len($lst['data']);
  if ($n == 0) {
  echo rtrim('No element found.'), PHP_EOL;
  return 0;
}
  $slow = 0;
  $fast = 0;
  while ($fast + 1 < $n) {
  $fast = $fast + 2;
  $slow = $slow + 1;
};
  return $lst['data'][$slow];
}
function main() {
  $lst = empty_list();
  middle_element($lst);
  $lst = push($lst, 5);
  echo rtrim(json_encode(5, 1344)), PHP_EOL;
  $lst = push($lst, 6);
  echo rtrim(json_encode(6, 1344)), PHP_EOL;
  $lst = push($lst, 8);
  echo rtrim(json_encode(8, 1344)), PHP_EOL;
  $lst = push($lst, 8);
  echo rtrim(json_encode(8, 1344)), PHP_EOL;
  $lst = push($lst, 10);
  echo rtrim(json_encode(10, 1344)), PHP_EOL;
  $lst = push($lst, 12);
  echo rtrim(json_encode(12, 1344)), PHP_EOL;
  $lst = push($lst, 17);
  echo rtrim(json_encode(17, 1344)), PHP_EOL;
  $lst = push($lst, 7);
  echo rtrim(json_encode(7, 1344)), PHP_EOL;
  $lst = push($lst, 3);
  echo rtrim(json_encode(3, 1344)), PHP_EOL;
  $lst = push($lst, 20);
  echo rtrim(json_encode(20, 1344)), PHP_EOL;
  $lst = push($lst, -20);
  echo rtrim(json_encode(-20, 1344)), PHP_EOL;
  echo rtrim(json_encode(middle_element($lst), 1344)), PHP_EOL;
}
main();

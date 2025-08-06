<?php
ini_set('memory_limit', '-1');
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$NULL = 0 - 1;
function empty_list() {
  global $NULL;
  return ['next' => [], 'head' => $NULL];
}
function add_node($list, $value) {
  global $NULL;
  $nexts = $list['next'];
  $new_index = count($nexts);
  $nexts = _append($nexts, $NULL);
  if ($list['head'] == $NULL) {
  return ['next' => $nexts, 'head' => $new_index];
}
  $last = $list['head'];
  while ($nexts[$last] != $NULL) {
  $last = $nexts[$last];
};
  $new_nexts = [];
  $i = 0;
  while ($i < count($nexts)) {
  if ($i == $last) {
  $new_nexts = _append($new_nexts, $new_index);
} else {
  $new_nexts = _append($new_nexts, $nexts[$i]);
}
  $i = $i + 1;
};
  return ['next' => $new_nexts, 'head' => $list['head']];
}
function set_next($list, $index, $next_index) {
  global $NULL;
  $nexts = $list['next'];
  $new_nexts = [];
  $i = 0;
  while ($i < count($nexts)) {
  if ($i == $index) {
  $new_nexts = _append($new_nexts, $next_index);
} else {
  $new_nexts = _append($new_nexts, $nexts[$i]);
}
  $i = $i + 1;
};
  return ['next' => $new_nexts, 'head' => $list['head']];
}
function detect_cycle($list) {
  global $NULL;
  if ($list['head'] == $NULL) {
  return false;
}
  $nexts = $list['next'];
  $slow = $list['head'];
  $fast = $list['head'];
  while ($fast != $NULL && $nexts[$fast] != $NULL) {
  $slow = $nexts[$slow];
  $fast = $nexts[$nexts[$fast]];
  if ($slow == $fast) {
  return true;
}
};
  return false;
}
function main() {
  global $NULL;
  $ll = empty_list();
  $ll = add_node($ll, 1);
  $ll = add_node($ll, 2);
  $ll = add_node($ll, 3);
  $ll = add_node($ll, 4);
  $ll = set_next($ll, 3, 1);
  echo rtrim(json_encode(detect_cycle($ll), 1344)), PHP_EOL;
}
main();

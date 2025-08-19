<?php
error_reporting(E_ALL & ~E_DEPRECATED);
ini_set('memory_limit', '-1');
function _str($x) {
    if (is_array($x)) {
        $isList = array_keys($x) === range(0, count($x) - 1);
        if ($isList) {
            $parts = [];
            foreach ($x as $v) { $parts[] = _str($v); }
            return '[' . implode(' ', $parts) . ']';
        }
        $parts = [];
        foreach ($x as $k => $v) { $parts[] = _str($k) . ':' . _str($v); }
        return 'map[' . implode(' ', $parts) . ']';
    }
    if (is_bool($x)) return $x ? 'true' : 'false';
    if ($x === null) return 'null';
    return strval($x);
}
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
function index_of($xs, $x) {
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
}
function majority_vote($votes, $votes_needed_to_win) {
  if ($votes_needed_to_win < 2) {
  return [];
}
  $candidates = [];
  $counts = [];
  $i = 0;
  while ($i < count($votes)) {
  $v = $votes[$i];
  $idx = index_of($candidates, $v);
  if ($idx != 0 - 1) {
  $counts[$idx] = $counts[$idx] + 1;
} else {
  if (count($candidates) < $votes_needed_to_win - 1) {
  $candidates = _append($candidates, $v);
  $counts = _append($counts, 1);
} else {
  $j = 0;
  while ($j < count($counts)) {
  $counts[$j] = $counts[$j] - 1;
  $j = $j + 1;
};
  $new_candidates = [];
  $new_counts = [];
  $j = 0;
  while ($j < count($candidates)) {
  if ($counts[$j] > 0) {
  $new_candidates = _append($new_candidates, $candidates[$j]);
  $new_counts = _append($new_counts, $counts[$j]);
}
  $j = $j + 1;
};
  $candidates = $new_candidates;
  $counts = $new_counts;
};
}
  $i = $i + 1;
};
  $final_counts = [];
  $j = 0;
  while ($j < count($candidates)) {
  $final_counts = _append($final_counts, 0);
  $j = $j + 1;
};
  $i = 0;
  while ($i < count($votes)) {
  $v = $votes[$i];
  $idx = index_of($candidates, $v);
  if ($idx != 0 - 1) {
  $final_counts[$idx] = $final_counts[$idx] + 1;
}
  $i = $i + 1;
};
  $result = [];
  $j = 0;
  while ($j < count($candidates)) {
  if ($final_counts[$j] * $votes_needed_to_win > count($votes)) {
  $result = _append($result, $candidates[$j]);
}
  $j = $j + 1;
};
  return $result;
}
function main() {
  $votes = [1, 2, 2, 3, 1, 3, 2];
  echo rtrim(_str(majority_vote($votes, 3))), PHP_EOL;
  echo rtrim(_str(majority_vote($votes, 2))), PHP_EOL;
  echo rtrim(_str(majority_vote($votes, 4))), PHP_EOL;
}
main();

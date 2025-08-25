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
function search_in_sorted_matrix($mat, $m, $n, $key) {
  $i = $m - 1;
  $j = 0;
  while ($i >= 0 && $j < $n) {
  if ($key == $mat[$i][$j]) {
  echo rtrim('Key ' . _str($key) . ' found at row- ' . _str($i + 1) . ' column- ' . _str($j + 1)), PHP_EOL;
  return;
}
  if ($key < $mat[$i][$j]) {
  $i = $i - 1;
} else {
  $j = $j + 1;
}
};
  echo rtrim('Key ' . _str($key) . ' not found'), PHP_EOL;
}
function main() {
  $mat = [[2.0, 5.0, 7.0], [4.0, 8.0, 13.0], [9.0, 11.0, 15.0], [12.0, 17.0, 20.0]];
  search_in_sorted_matrix($mat, count($mat), count($mat[0]), 5.0);
  search_in_sorted_matrix($mat, count($mat), count($mat[0]), 21.0);
  $mat2 = [[2.1, 5.0, 7.0], [4.0, 8.0, 13.0], [9.0, 11.0, 15.0], [12.0, 17.0, 20.0]];
  search_in_sorted_matrix($mat2, count($mat2), count($mat2[0]), 2.1);
  search_in_sorted_matrix($mat2, count($mat2), count($mat2[0]), 2.2);
}
main();

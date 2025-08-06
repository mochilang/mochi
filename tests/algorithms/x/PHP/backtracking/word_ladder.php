<?php
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
$alphabet = 'abcdefghijklmnopqrstuvwxyz';
function contains($xs, $x) {
  global $alphabet;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $x) {
  return true;
}
  $i = $i + 1;
};
  return false;
}
function remove_item($xs, $x) {
  global $alphabet;
  $res = [];
  $removed = false;
  $i = 0;
  while ($i < count($xs)) {
  if (!$removed && $xs[$i] == $x) {
  $removed = true;
} else {
  $res = _append($res, $xs[$i]);
}
  $i = $i + 1;
};
  return $res;
}
function word_ladder($current, $path, $target, $words) {
  global $alphabet;
  if ($current == $target) {
  return $path;
}
  $i = 0;
  while ($i < strlen($current)) {
  $j = 0;
  while ($j < strlen($alphabet)) {
  $c = substr($alphabet, $j, $j + 1 - $j);
  $transformed = substr($current, 0, $i - 0) . $c . substr($current, $i + 1, strlen($current) - ($i + 1));
  if (in_array($transformed, $words)) {
  $new_words = remove_item($words, $transformed);
  $new_path = _append($path, $transformed);
  $result = word_ladder($transformed, $new_path, $target, $new_words);
  if (count($result) > 0) {
  return $result;
};
}
  $j = $j + 1;
};
  $i = $i + 1;
};
  return [];
}
function main() {
  global $alphabet;
  $w1 = ['hot', 'dot', 'dog', 'lot', 'log', 'cog'];
  echo rtrim(_str(word_ladder('hit', ['hit'], 'cog', $w1))), PHP_EOL;
  $w2 = ['hot', 'dot', 'dog', 'lot', 'log'];
  echo rtrim(_str(word_ladder('hit', ['hit'], 'cog', $w2))), PHP_EOL;
  $w3 = ['load', 'goad', 'gold', 'lead', 'lord'];
  echo rtrim(_str(word_ladder('lead', ['lead'], 'gold', $w3))), PHP_EOL;
  $w4 = ['came', 'cage', 'code', 'cade', 'gave'];
  echo rtrim(_str(word_ladder('game', ['game'], 'code', $w4))), PHP_EOL;
}
main();

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
function contains($words, $target) {
  foreach ($words as $w) {
  if ($w == $target) {
  return true;
}
};
  return false;
}
function backtrack($s, $word_dict, $start) {
  if ($start == strlen($s)) {
  return true;
}
  $end = $start + 1;
  while ($end <= strlen($s)) {
  $substr = substr($s, $start, $end - $start);
  if (in_array($substr, $word_dict) && backtrack($s, $word_dict, $end)) {
  return true;
}
  $end = $end + 1;
};
  return false;
}
function word_break($s, $word_dict) {
  return backtrack($s, $word_dict, 0);
}
echo rtrim(_str(word_break('leetcode', ['leet', 'code']))), PHP_EOL;
echo rtrim(_str(word_break('applepenapple', ['apple', 'pen']))), PHP_EOL;
echo rtrim(_str(word_break('catsandog', ['cats', 'dog', 'sand', 'and', 'cat']))), PHP_EOL;

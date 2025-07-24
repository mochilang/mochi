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
function indexOfStr($h, $n) {
  global $stringSearchSingle, $stringSearch, $display, $main;
  $hlen = strlen($h);
  $nlen = strlen($n);
  if ($nlen == 0) {
  return 0;
}
  $i = 0;
  while ($i <= $hlen - $nlen) {
  if (substr($h, $i, $i + $nlen - $i) == $n) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function stringSearchSingle($h, $n) {
  global $indexOfStr, $stringSearch, $display, $main;
  return indexOfStr($h, $n);
}
function stringSearch($h, $n) {
  global $indexOfStr, $stringSearchSingle, $display, $main;
  $result = [];
  $start = 0;
  $hlen = strlen($h);
  $nlen = strlen($n);
  while ($start < $hlen) {
  $idx = indexOfStr(substr($h, $start, $hlen - $start), $n);
  if ($idx >= 0) {
  $result = array_merge($result, [$start + $idx]);
  $start = $start + $idx + $nlen;
} else {
  break;
}
};
  return $result;
}
function display($nums) {
  global $indexOfStr, $stringSearchSingle, $stringSearch, $main;
  $s = '[';
  $i = 0;
  while ($i < count($nums)) {
  if ($i > 0) {
  $s = $s . ', ';
}
  $s = $s . _str($nums[$i]);
  $i = $i + 1;
};
  $s = $s . ']';
  return $s;
}
function main() {
  global $indexOfStr, $stringSearchSingle, $stringSearch, $display;
  $texts = ['GCTAGCTCTACGAGTCTA', 'GGCTATAATGCGTA', 'there would have been a time for such a word', 'needle need noodle needle', 'DKnuthusesandprogramsanimaginarycomputertheMIXanditsassociatedmachinecodeandassemblylanguages', 'Nearby farms grew an acre of alfalfa on the dairy\'s behalf, with bales of that alfalfa exchanged for milk.'];
  $patterns = ['TCTA', 'TAATAAA', 'word', 'needle', 'and', 'alfalfa'];
  $i = 0;
  while ($i < count($texts)) {
  echo rtrim('text' . _str($i + 1) . ' = ' . $texts[$i]), PHP_EOL;
  $i = $i + 1;
};
  echo rtrim(''), PHP_EOL;
  $j = 0;
  while ($j < count($texts)) {
  $idxs = stringSearch($texts[$j], $patterns[$j]);
  echo rtrim('Found "' . $patterns[$j] . '" in \'text' . _str($j + 1) . '\' at indexes ' . display($idxs)), PHP_EOL;
  $j = $j + 1;
};
}
main();

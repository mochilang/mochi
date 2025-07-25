<?php
ini_set('memory_limit', '-1');
$now_seed = 0;
$now_seeded = false;
$s = getenv('MOCHI_NOW_SEED');
if ($s !== false && $s !== '') {
    $now_seed = intval($s);
    $now_seeded = true;
}
function _now() {
    global $now_seed, $now_seeded;
    if ($now_seeded) {
        $now_seed = ($now_seed * 1664525 + 1013904223) % 2147483647;
        return $now_seed;
    }
    return hrtime(true);
}
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
$__start_mem = memory_get_usage();
$__start = _now();
  function indexOfStr($h, $n) {
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
};
  function stringSearchSingle($h, $n) {
  return indexOfStr($h, $n);
};
  function stringSearch($h, $n) {
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
};
  function display($nums) {
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
};
  function main() {
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
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;

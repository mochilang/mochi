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
  function square_to_maps($square) {
  $emap = [];
  $dmap = [];
  $x = 0;
  while ($x < count($square)) {
  $row = $square[$x];
  $y = 0;
  while ($y < count($row)) {
  $ch = $row[$y];
  $emap[$ch] = [$x, $y];
  $dmap[_str($x) . ',' . _str($y)] = $ch;
  $y = $y + 1;
};
  $x = $x + 1;
};
  return ['e' => $emap, 'd' => $dmap];
};
  function remove_space($text, $emap) {
  $s = strtoupper($text);
  $out = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch != ' ' && array_key_exists($ch, $emap)) {
  $out = $out . $ch;
}
  $i = $i + 1;
};
  return $out;
};
  function encrypt($text, $emap, $dmap) {
  $text = remove_space($text, $emap);
  $row0 = [];
  $row1 = [];
  $i = 0;
  while ($i < strlen($text)) {
  $ch = substr($text, $i, $i + 1 - $i);
  $xy = $emap[$ch];
  $row0 = array_merge($row0, [$xy[0]]);
  $row1 = array_merge($row1, [$xy[1]]);
  $i = $i + 1;
};
  foreach ($row1 as $v) {
  $row0 = array_merge($row0, [$v]);
};
  $res = '';
  $j = 0;
  while ($j < count($row0)) {
  $key = _str($row0[$j]) . ',' . _str($row0[$j + 1]);
  $res = $res . $dmap[$key];
  $j = $j + 2;
};
  return $res;
};
  function decrypt($text, $emap, $dmap) {
  $text = remove_space($text, $emap);
  $coords = [];
  $i = 0;
  while ($i < strlen($text)) {
  $ch = substr($text, $i, $i + 1 - $i);
  $xy = $emap[$ch];
  $coords = array_merge($coords, [$xy[0]]);
  $coords = array_merge($coords, [$xy[1]]);
  $i = $i + 1;
};
  $half = count($coords) / 2;
  $k1 = [];
  $k2 = [];
  $idx = 0;
  while ($idx < $half) {
  $k1 = array_merge($k1, [$coords[$idx]]);
  $idx = $idx + 1;
};
  while ($idx < count($coords)) {
  $k2 = array_merge($k2, [$coords[$idx]]);
  $idx = $idx + 1;
};
  $res = '';
  $j = 0;
  while ($j < $half) {
  $key = _str($k1[$j]) . ',' . _str($k2[$j]);
  $res = $res . $dmap[$key];
  $j = $j + 1;
};
  return $res;
};
  function main() {
  $squareRosetta = [['A', 'B', 'C', 'D', 'E'], ['F', 'G', 'H', 'I', 'K'], ['L', 'M', 'N', 'O', 'P'], ['Q', 'R', 'S', 'T', 'U'], ['V', 'W', 'X', 'Y', 'Z'], ['J', '1', '2', '3', '4']];
  $squareWikipedia = [['B', 'G', 'W', 'K', 'Z'], ['Q', 'P', 'N', 'D', 'S'], ['I', 'O', 'A', 'X', 'E'], ['F', 'C', 'L', 'U', 'M'], ['T', 'H', 'Y', 'V', 'R'], ['J', '1', '2', '3', '4']];
  $textRosetta = '0ATTACKATDAWN';
  $textWikipedia = 'FLEEATONCE';
  $textTest = 'The invasion will start on the first of January';
  $maps = square_to_maps($squareRosetta);
  $emap = $maps['e'];
  $dmap = $maps['d'];
  echo rtrim('from Rosettacode'), PHP_EOL;
  echo rtrim('original:\t ' . $textRosetta), PHP_EOL;
  $s = encrypt($textRosetta, $emap, $dmap);
  echo rtrim('codiert:\t ' . $s), PHP_EOL;
  $s = decrypt($s, $emap, $dmap);
  echo rtrim('and back:\t ' . $s), PHP_EOL;
  $maps = square_to_maps($squareWikipedia);
  $emap = $maps['e'];
  $dmap = $maps['d'];
  echo rtrim('from Wikipedia'), PHP_EOL;
  echo rtrim('original:\t ' . $textWikipedia), PHP_EOL;
  $s = encrypt($textWikipedia, $emap, $dmap);
  echo rtrim('codiert:\t ' . $s), PHP_EOL;
  $s = decrypt($s, $emap, $dmap);
  echo rtrim('and back:\t ' . $s), PHP_EOL;
  $maps = square_to_maps($squareWikipedia);
  $emap = $maps['e'];
  $dmap = $maps['d'];
  echo rtrim('from Rosettacode long part'), PHP_EOL;
  echo rtrim('original:\t ' . $textTest), PHP_EOL;
  $s = encrypt($textTest, $emap, $dmap);
  echo rtrim('codiert:\t ' . $s), PHP_EOL;
  $s = decrypt($s, $emap, $dmap);
  echo rtrim('and back:\t ' . $s), PHP_EOL;
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

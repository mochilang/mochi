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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $adfgvx = 'ADFGVX';
  $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  function shuffleStr($s) {
  global $adfgvx, $alphabet;
  $arr = [];
  $i = 0;
  while ($i < strlen($s)) {
  $arr = array_merge($arr, [substr($s, $i, $i + 1 - $i)]);
  $i = $i + 1;
};
  $j = count($arr) - 1;
  while ($j > 0) {
  $k = _now() % ($j + 1);
  $tmp = $arr[$j];
  $arr[$j] = $arr[$k];
  $arr[$k] = $tmp;
  $j = $j - 1;
};
  $out = '';
  $i = 0;
  while ($i < count($arr)) {
  $out = $out . $arr[$i];
  $i = $i + 1;
};
  return $out;
};
  function createPolybius() {
  global $adfgvx, $alphabet;
  $shuffled = shuffleStr($alphabet);
  $labels = [];
  $li = 0;
  while ($li < strlen($adfgvx)) {
  $labels = array_merge($labels, [substr($adfgvx, $li, $li + 1 - $li)]);
  $li = $li + 1;
};
  echo rtrim('6 x 6 Polybius square:\n'), PHP_EOL;
  echo rtrim('  | A D F G V X'), PHP_EOL;
  echo rtrim('---------------'), PHP_EOL;
  $p = [];
  $i = 0;
  while ($i < 6) {
  $row = substr($shuffled, $i * 6, ($i + 1) * 6 - $i * 6);
  $p = array_merge($p, [$row]);
  $line = $labels[$i] . ' | ';
  $j = 0;
  while ($j < 6) {
  $line = $line . substr($row, $j, $j + 1 - $j) . ' ';
  $j = $j + 1;
};
  echo rtrim($line), PHP_EOL;
  $i = $i + 1;
};
  return $p;
};
  function createKey($n) {
  global $adfgvx, $alphabet;
  if ($n < 7 || $n > 12) {
  echo rtrim('Key should be within 7 and 12 letters long.'), PHP_EOL;
}
  $pool = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  $key = '';
  $i = 0;
  while ($i < $n) {
  $idx = _now() % strlen($pool);
  $key = $key . substr($pool, $idx, $idx + 1 - $idx);
  $pool = substr($pool, 0, $idx - 0) . substr($pool, $idx + 1, strlen($pool) - ($idx + 1));
  $i = $i + 1;
};
  echo rtrim('\nThe key is ' . $key), PHP_EOL;
  return $key;
};
  function orderKey($key) {
  global $adfgvx, $alphabet;
  $pairs = [];
  $i = 0;
  while ($i < strlen($key)) {
  $pairs = array_merge($pairs, [[substr($key, $i, $i + 1 - $i), $i]]);
  $i = $i + 1;
};
  $n = count($pairs);
  $m = 0;
  while ($m < $n) {
  $j = 0;
  while ($j < $n - 1) {
  if ($pairs[$j][0] > $pairs[$j + 1][0]) {
  $tmp = $pairs[$j];
  $pairs[$j] = $pairs[$j + 1];
  $pairs[$j + 1] = $tmp;
}
  $j = $j + 1;
};
  $m = $m + 1;
};
  $res = [];
  $i = 0;
  while ($i < $n) {
  $res = array_merge($res, [ord($pairs[$i][1])]);
  $i = $i + 1;
};
  return $res;
};
  function encrypt($polybius, $key, $plainText) {
  global $adfgvx, $alphabet;
  $labels = [];
  $li = 0;
  while ($li < strlen($adfgvx)) {
  $labels = array_merge($labels, [substr($adfgvx, $li, $li + 1 - $li)]);
  $li = $li + 1;
};
  $temp = '';
  $i = 0;
  while ($i < strlen($plainText)) {
  $r = 0;
  while ($r < 6) {
  $c = 0;
  while ($c < 6) {
  if (substr($polybius[$r], $c, $c + 1 - $c) == substr($plainText, $i, $i + 1 - $i)) {
  $temp = $temp . $labels[$r] . $labels[$c];
}
  $c = $c + 1;
};
  $r = $r + 1;
};
  $i = $i + 1;
};
  $colLen = strlen($temp) / strlen($key);
  if (strlen($temp) % strlen($key) > 0) {
  $colLen = $colLen + 1;
}
  $table = [];
  $rIdx = 0;
  while ($rIdx < $colLen) {
  $row = [];
  $j = 0;
  while ($j < strlen($key)) {
  $row = array_merge($row, ['']);
  $j = $j + 1;
};
  $table = array_merge($table, [$row]);
  $rIdx = $rIdx + 1;
};
  $idx = 0;
  while ($idx < strlen($temp)) {
  $row = $idx / strlen($key);
  $col = $idx % strlen($key);
  $table[$row][$col] = substr($temp, $idx, $idx + 1 - $idx);
  $idx = $idx + 1;
};
  $order = orderKey($key);
  $cols = [];
  $ci = 0;
  while ($ci < strlen($key)) {
  $colStr = '';
  $ri = 0;
  while ($ri < $colLen) {
  $colStr = $colStr . $table[$ri][$order[$ci]];
  $ri = $ri + 1;
};
  $cols = array_merge($cols, [$colStr]);
  $ci = $ci + 1;
};
  $result = '';
  $ci = 0;
  while ($ci < count($cols)) {
  $result = $result . $cols[$ci];
  if ($ci < count($cols) - 1) {
  $result = $result . ' ';
}
  $ci = $ci + 1;
};
  return $result;
};
  function indexOf($s, $ch) {
  global $adfgvx, $alphabet;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
};
  function decrypt($polybius, $key, $cipherText) {
  global $adfgvx, $alphabet;
  $colStrs = [];
  $start = 0;
  $i = 0;
  while ($i <= strlen($cipherText)) {
  if ($i == strlen($cipherText) || substr($cipherText, $i, $i + 1 - $i) == ' ') {
  $colStrs = array_merge($colStrs, [substr($cipherText, $start, $i - $start)]);
  $start = $i + 1;
}
  $i = $i + 1;
};
  $maxColLen = 0;
  $i = 0;
  while ($i < count($colStrs)) {
  if (strlen($colStrs[$i]) > $maxColLen) {
  $maxColLen = strlen($colStrs[$i]);
}
  $i = $i + 1;
};
  $cols = [];
  $i = 0;
  while ($i < count($colStrs)) {
  $s = $colStrs[$i];
  $ls = [];
  $j = 0;
  while ($j < strlen($s)) {
  $ls = array_merge($ls, [substr($s, $j, $j + 1 - $j)]);
  $j = $j + 1;
};
  if (strlen($s) < $maxColLen) {
  $pad = [];
  $k = 0;
  while ($k < $maxColLen) {
  if ($k < count($ls)) {
  $pad = array_merge($pad, [$ls[$k]]);
} else {
  $pad = array_merge($pad, ['']);
}
  $k = $k + 1;
};
  $cols = array_merge($cols, [$pad]);
} else {
  $cols = array_merge($cols, [$ls]);
}
  $i = $i + 1;
};
  $table = [];
  $r = 0;
  while ($r < $maxColLen) {
  $row = [];
  $c = 0;
  while ($c < strlen($key)) {
  $row = array_merge($row, ['']);
  $c = $c + 1;
};
  $table = array_merge($table, [$row]);
  $r = $r + 1;
};
  $order = orderKey($key);
  $r = 0;
  while ($r < $maxColLen) {
  $c = 0;
  while ($c < strlen($key)) {
  $table[$r][$order[$c]] = $cols[$c][$r];
  $c = $c + 1;
};
  $r = $r + 1;
};
  $temp = '';
  $r = 0;
  while ($r < count($table)) {
  $j = 0;
  while ($j < count($table[$r])) {
  $temp = $temp . $table[$r][$j];
  $j = $j + 1;
};
  $r = $r + 1;
};
  $plainText = '';
  $idx = 0;
  while ($idx < strlen($temp)) {
  $rIdx = _indexof($adfgvx, substr($temp, $idx, $idx + 1 - $idx));
  $cIdx = _indexof($adfgvx, substr($temp, $idx + 1, $idx + 2 - ($idx + 1)));
  $plainText = $plainText . substr($polybius[$rIdx], $cIdx, $cIdx + 1 - $cIdx);
  $idx = $idx + 2;
};
  return $plainText;
};
  function main() {
  global $adfgvx, $alphabet;
  $plainText = 'ATTACKAT1200AM';
  $polybius = createPolybius();
  $key = createKey(9);
  echo rtrim('\nPlaintext : ' . $plainText), PHP_EOL;
  $cipherText = encrypt($polybius, $key, $plainText);
  echo rtrim('\nEncrypted : ' . $cipherText), PHP_EOL;
  $plainText2 = decrypt($polybius, $key, $cipherText);
  echo rtrim('\nDecrypted : ' . $plainText2), PHP_EOL;
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

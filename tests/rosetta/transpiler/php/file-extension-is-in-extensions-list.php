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
  function endsWith($s, $suf) {
  global $extensions;
  if (strlen($s) < strlen($suf)) {
  return false;
}
  return substr($s, strlen($s) - strlen($suf), strlen($s) - (strlen($s) - strlen($suf))) == $suf;
};
  function lastIndexOf($s, $sub) {
  global $extensions;
  $idx = 0 - 1;
  $i = 0;
  while ($i <= strlen($s) - strlen($sub)) {
  if (substr($s, $i, $i + strlen($sub) - $i) == $sub) {
  $idx = $i;
}
  $i = $i + 1;
};
  return $idx;
};
  $extensions = ['zip', 'rar', '7z', 'gz', 'archive', 'A##', 'tar.bz2'];
  function fileExtInList($filename) {
  global $extensions;
  $fl = strtolower($filename);
  foreach ($extensions as $ext) {
  $ext2 = '.' . strtolower($ext);
  if (endsWith($fl, $ext2)) {
  return [true, $ext];
}
};
  $idx = lastIndexOf($filename, '.');
  if ($idx != 0 - 1) {
  $t = substr($filename, $idx + 1, strlen($filename) - ($idx + 1));
  if ($t != '') {
  return [false, $t];
};
  return [false, '<empty>'];
}
  return [false, '<none>'];
};
  function pad($s, $w) {
  global $extensions;
  $t = $s;
  while (strlen($t) < $w) {
  $t = $t . ' ';
};
  return $t;
};
  function main() {
  global $extensions;
  echo rtrim('The listed extensions are:'), PHP_EOL;
  echo rtrim(str_replace('false', 'False', str_replace('true', 'True', str_replace('"', '\'', str_replace(':', ': ', str_replace(',', ', ', json_encode($extensions, 1344))))))), PHP_EOL;
  $tests = ['MyData.a##', 'MyData.tar.Gz', 'MyData.gzip', 'MyData.7z.backup', 'MyData...', 'MyData', 'MyData_v1.0.tar.bz2', 'MyData_v1.0.bz2'];
  foreach ($tests as $t) {
  $res = fileExtInList($t);
  $ok = boolval($res[0]);
  $ext = strval($res[1]);
  echo rtrim(pad($t, 20) . ' => ' . _str($ok) . '  (extension = ' . $ext . ')'), PHP_EOL;
};
};
  main();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;

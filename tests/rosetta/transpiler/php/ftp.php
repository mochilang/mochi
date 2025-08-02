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
function _len($x) {
    if (is_array($x)) { return count($x); }
    if (is_string($x)) { return strlen($x); }
    return strlen(strval($x));
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
  $serverData = ['pub' => ['somefile.bin' => 'This is a file from the FTP server.', 'readme.txt' => 'Hello from ftp.']];
  $serverNames = ['pub' => ['somefile.bin', 'readme.txt']];
  function connect($hostport) {
  global $serverData, $serverNames;
  echo rtrim('Connected to ' . $hostport), PHP_EOL;
  return ['dir' => '/'];
};
  function login($conn, $user, $pass) {
  global $serverData, $serverNames;
  echo rtrim('Logged in as ' . $user), PHP_EOL;
};
  function changeDir(&$conn, $dir) {
  global $serverData, $serverNames;
  $conn['dir'] = $dir;
};
  function mochi_list($conn) {
  global $serverData, $serverNames;
  $names = $serverNames[$conn['dir']];
  $dataDir = $serverData[$conn['dir']];
  $out = [];
  foreach ($names as $name) {
  $content = $dataDir[$name];
  $out = array_merge($out, [['name' => $name, 'size' => _len($content), 'kind' => 'file']]);
};
  return $out;
};
  function retrieve($conn, $name) {
  global $serverData, $serverNames;
  return $serverData[$conn['dir']][$name];
};
  function main() {
  global $serverData, $serverNames;
  $conn = connect('localhost:21');
  login($conn, 'anonymous', 'anonymous');
  changeDir($conn, 'pub');
  echo rtrim(json_encode($conn['dir'], 1344)), PHP_EOL;
  $files = mochi_list($conn);
  foreach ($files as $f) {
  echo rtrim($f['name'] . ' ' . _str($f['size'])), PHP_EOL;
};
  $data = retrieve($conn, 'somefile.bin');
  echo rtrim('Wrote ' . _str(strlen($data)) . ' bytes to somefile.bin'), PHP_EOL;
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

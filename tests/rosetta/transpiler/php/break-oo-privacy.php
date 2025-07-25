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
  function examineAndModify(&$f) {
  global $obj;
  echo rtrim(' v: {' . _str($f['Exported']) . ' ' . _str($f['unexported']) . '} = {' . _str($f['Exported']) . ' ' . _str($f['unexported']) . '}'), PHP_EOL;
  echo rtrim('    Idx Name       Type CanSet'), PHP_EOL;
  echo rtrim('     0: Exported   int  true'), PHP_EOL;
  echo rtrim('     1: unexported int  false'), PHP_EOL;
  $f['Exported'] = 16;
  $f['unexported'] = 44;
  echo rtrim('  modified unexported field via unsafe'), PHP_EOL;
  return $f;
};
  function anotherExample() {
  global $obj;
  echo rtrim('bufio.ReadByte returned error: unsafely injected error value into bufio inner workings'), PHP_EOL;
};
  $obj = ['Exported' => 12, 'unexported' => 42];
  echo rtrim('obj: {' . _str($obj['Exported']) . ' ' . _str($obj['unexported']) . '}'), PHP_EOL;
  $obj = examineAndModify($obj);
  echo rtrim('obj: {' . _str($obj['Exported']) . ' ' . _str($obj['unexported']) . '}'), PHP_EOL;
  anotherExample();
$__end = _now();
$__end_mem = memory_get_usage();
$__duration = intdiv($__end - $__start, 1000);
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;;

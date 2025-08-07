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
function _panic($msg) {
    fwrite(STDERR, strval($msg));
    exit(1);
}
$__start_mem = memory_get_usage();
$__start = _now();
  function focal_length_of_lens($object_distance_from_lens, $image_distance_from_lens) {
  if ($object_distance_from_lens == 0.0 || $image_distance_from_lens == 0.0) {
  _panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
}
  return 1.0 / ((1.0 / $image_distance_from_lens) - (1.0 / $object_distance_from_lens));
};
  function object_distance($focal_length_of_lens, $image_distance_from_lens) {
  if ($image_distance_from_lens == 0.0 || 'focal_length_of_lens' == 0.0) {
  _panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
}
  return 1.0 / ((1.0 / $image_distance_from_lens) - (1.0 / 'focal_length_of_lens'));
};
  function image_distance($focal_length_of_lens, $object_distance_from_lens) {
  if ($object_distance_from_lens == 0.0 || 'focal_length_of_lens' == 0.0) {
  _panic('Invalid inputs. Enter non zero values with respect to the sign convention.');
}
  return 1.0 / ((1.0 / $object_distance_from_lens) + (1.0 / 'focal_length_of_lens'));
};
  echo rtrim(_str(focal_length_of_lens(10.0, 4.0))), PHP_EOL;
  echo rtrim(_str(focal_length_of_lens(2.7, 5.8))), PHP_EOL;
  echo rtrim(_str(object_distance(10.0, 40.0))), PHP_EOL;
  echo rtrim(_str(object_distance(6.2, 1.5))), PHP_EOL;
  echo rtrim(_str(image_distance(50.0, 40.0))), PHP_EOL;
  echo rtrim(_str(image_distance(5.3, 7.9))), PHP_EOL;
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;

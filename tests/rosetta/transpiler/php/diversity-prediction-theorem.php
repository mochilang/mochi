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
  function pow10($n) {
  $r = 1.0;
  $i = 0;
  while ($i < $n) {
  $r = $r * 10.0;
  $i = $i + 1;
};
  return $r;
};
  function formatFloat($f, $prec) {
  $scale = pow10($prec);
  $scaled = ($f * $scale) + 0.5;
  $n = (intval($scaled));
  $digits = _str($n);
  while (strlen($digits) <= $prec) {
  $digits = '0' . $digits;
};
  $intPart = substr($digits, 0, strlen($digits) - $prec - 0);
  $fracPart = substr($digits, strlen($digits) - $prec, strlen($digits) - (strlen($digits) - $prec));
  return $intPart . '.' . $fracPart;
};
  function padLeft($s, $w) {
  $res = '';
  $n = $w - strlen($s);
  while ($n > 0) {
  $res = $res . ' ';
  $n = $n - 1;
};
  return $res . $s;
};
  function averageSquareDiff($f, $preds) {
  $av = 0.0;
  $i = 0;
  while ($i < count($preds)) {
  $av = $av + ($preds[$i] - $f) * ($preds[$i] - $f);
  $i = $i + 1;
};
  $av = $av / (floatval(count($preds)));
  return $av;
};
  function diversityTheorem($truth, $preds) {
  $av = 0.0;
  $i = 0;
  while ($i < count($preds)) {
  $av = $av + $preds[$i];
  $i = $i + 1;
};
  $av = $av / (floatval(count($preds)));
  $avErr = averageSquareDiff($truth, $preds);
  $crowdErr = ($truth - $av) * ($truth - $av);
  $div = averageSquareDiff($av, $preds);
  return [$avErr, $crowdErr, $div];
};
  function main() {
  $predsArray = [[48.0, 47.0, 51.0], [48.0, 47.0, 51.0, 42.0]];
  $truth = 49.0;
  $i = 0;
  while ($i < count($predsArray)) {
  $preds = $predsArray[$i];
  $res = diversityTheorem($truth, $preds);
  echo rtrim('Average-error : ' . padLeft(formatFloat($res[0], 3), 6)), PHP_EOL;
  echo rtrim('Crowd-error   : ' . padLeft(formatFloat($res[1], 3), 6)), PHP_EOL;
  echo rtrim('Diversity     : ' . padLeft(formatFloat($res[2], 3), 6)), PHP_EOL;
  echo rtrim(''), PHP_EOL;
  $i = $i + 1;
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

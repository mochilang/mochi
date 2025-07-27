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
  function TinyInt_Add($self, $t2) {
  $value = $self['value'];
  return NewTinyInt($value + $t2['value']);
};
  function TinyInt_Sub($self, $t2) {
  $value = $self['value'];
  return NewTinyInt($value - $t2['value']);
};
  function TinyInt_Mul($self, $t2) {
  $value = $self['value'];
  return NewTinyInt($value * $t2['value']);
};
  function TinyInt_Div($self, $t2) {
  $value = $self['value'];
  return NewTinyInt($value / $t2['value']);
};
  function TinyInt_Rem($self, $t2) {
  $value = $self['value'];
  return NewTinyInt(fmod($value, $t2['value']));
};
  function TinyInt_Inc($self) {
  $value = $self['value'];
  return TinyInt_Add($self, NewTinyInt(1));
};
  function TinyInt_Dec($self) {
  $value = $self['value'];
  return TinyInt_Sub($self, NewTinyInt(1));
};
  function NewTinyInt($i) {
  if ($i < 1) {
  $i = 1;
} else {
  if ($i > 10) {
  $i = 10;
};
}
  return ['value' => $i];
};
  function main() {
  $t1 = NewTinyInt(6);
  $t2 = NewTinyInt(3);
  echo rtrim('t1      = ' . _str($t1['value'])), PHP_EOL;
  echo rtrim('t2      = ' . _str($t2['value'])), PHP_EOL;
  echo rtrim('t1 + t2 = ' . _str(TinyInt_Add($t1, $t2))), PHP_EOL;
  echo rtrim('t1 - t2 = ' . _str(TinyInt_Sub($t1, $t2))), PHP_EOL;
  echo rtrim('t1 * t2 = ' . _str(TinyInt_Mul($t1, $t2))), PHP_EOL;
  echo rtrim('t1 / t2 = ' . _str(TinyInt_Div($t1, $t2))), PHP_EOL;
  echo rtrim('t1 % t2 = ' . _str(TinyInt_Rem($t1, $t2))), PHP_EOL;
  echo rtrim('t1 + 1  = ' . _str(TinyInt_Inc($t1))), PHP_EOL;
  echo rtrim('t1 - 1  = ' . _str(TinyInt_Dec($t1))), PHP_EOL;
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

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
function isNumeric($s) {
  if ($s == 'NaN') {
  return true;
}
  $i = 0;
  if (strlen($s) == 0) {
  return false;
}
  if (substr($s, 0, 0 + 1 - 0) == '+' || substr($s, 0, 0 + 1 - 0) == '-') {
  if (strlen($s) == 1) {
  return false;
};
  $i = 1;
}
  $digits = false;
  $dot = false;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch >= '0' && $ch <= '9') {
  $digits = true;
  $i = $i + 1;
} else {
  if ($ch == '.' && $dot == false) {
  $dot = true;
  $i = $i + 1;
} else {
  if (($ch == 'e' || $ch == 'E') && $digits) {
  $i = $i + 1;
  if ($i < strlen($s) && (substr($s, $i, $i + 1 - $i) == '+' || substr($s, $i, $i + 1 - $i) == '-')) {
  $i = $i + 1;
};
  $ed = false;
  while ($i < strlen($s) && substr($s, $i, $i + 1 - $i) >= '0' && substr($s, $i, $i + 1 - $i) <= '9') {
  $ed = true;
  $i = $i + 1;
};
  return $ed && $i == strlen($s);
} else {
  return false;
};
};
}
};
  return $digits;
}
function main() {
  echo rtrim('Are these strings numeric?'), PHP_EOL;
  $strs = ['1', '3.14', '-100', '1e2', 'NaN', 'rose'];
  foreach ($strs as $s) {
  echo rtrim('  ' . $s . ' -> ' . _str(isNumeric($s))), PHP_EOL;
};
}
main();

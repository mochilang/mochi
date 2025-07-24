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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
function isPrime($n) {
  global $firstPrimeFactor, $indexOf, $padLeft, $formatFloat, $main;
  if ($n < 2) {
  return false;
}
  if ($n % 2 == 0) {
  return $n == 2;
}
  if ($n % 3 == 0) {
  return $n == 3;
}
  $d = 5;
  while ($d * $d <= $n) {
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 2;
  if ($n % $d == 0) {
  return false;
}
  $d = $d + 4;
};
  return true;
}
function firstPrimeFactor($n) {
  global $isPrime, $indexOf, $padLeft, $formatFloat, $main;
  if ($n == 1) {
  return 1;
}
  if ($n % 3 == 0) {
  return 3;
}
  if ($n % 5 == 0) {
  return 5;
}
  $inc = [4, 2, 4, 2, 4, 6, 2, 6];
  $k = 7;
  $i = 0;
  while ($k * $k <= $n) {
  if ($n % $k == 0) {
  return $k;
}
  $k = $k + $inc[$i];
  $i = ($i + 1) % count($inc);
};
  return $n;
}
function indexOf($s, $ch) {
  global $isPrime, $firstPrimeFactor, $padLeft, $formatFloat, $main;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function padLeft($n, $width) {
  global $isPrime, $firstPrimeFactor, $indexOf, $formatFloat, $main;
  $s = _str($n);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
}
function formatFloat($f, $prec) {
  global $isPrime, $firstPrimeFactor, $indexOf, $padLeft, $main;
  $s = _str($f);
  $idx = _indexof($s, '.');
  if ($idx < 0) {
  return $s;
}
  $need = $idx + 1 + $prec;
  if (strlen($s) > $need) {
  return substr($s, 0, $need - 0);
}
  return $s;
}
function main() {
  global $isPrime, $firstPrimeFactor, $indexOf, $padLeft, $formatFloat;
  $blum = [];
  $counts = [0, 0, 0, 0];
  $digits = [1, 3, 7, 9];
  $i = 1;
  $bc = 0;
  while (true) {
  $p = firstPrimeFactor($i);
  if ($p % 4 == 3) {
  $q = intval((intdiv($i, $p)));
  if ($q != $p && $q % 4 == 3 && isPrime($q)) {
  if ($bc < 50) {
  $blum = array_merge($blum, [$i]);
};
  $d = $i % 10;
  if ($d == 1) {
  $counts[0] = $counts[0] + 1;
} else {
  if ($d == 3) {
  $counts[1] = $counts[1] + 1;
} else {
  if ($d == 7) {
  $counts[2] = $counts[2] + 1;
} else {
  if ($d == 9) {
  $counts[3] = $counts[3] + 1;
};
};
};
};
  $bc = $bc + 1;
  if ($bc == 50) {
  echo rtrim('First 50 Blum integers:'), PHP_EOL;
  $idx = 0;
  while ($idx < 50) {
  $line = '';
  $j = 0;
  while ($j < 10) {
  $line = $line . padLeft($blum[$idx], 3) . ' ';
  $idx = $idx + 1;
  $j = $j + 1;
};
  echo rtrim(json_encode(substr($line, 0, strlen($line) - 1 - 0), 1344)), PHP_EOL;
};
  break;
};
};
}
  if ($i % 5 == 3) {
  $i = $i + 4;
} else {
  $i = $i + 2;
}
};
}
main();

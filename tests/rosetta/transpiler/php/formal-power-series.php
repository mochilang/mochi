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
function _indexof($s, $sub) {
    $pos = strpos($s, $sub);
    return $pos === false ? -1 : $pos;
}
$__start_mem = memory_get_usage();
$__start = _now();
  function newFps($fn) {
  return ['coeffs' => [], 'compute' => $fn];
};
  function mochi_extract(&$f, $n) {
  while (_len($f['coeffs']) <= $n) {
  $idx = _len($f['coeffs']);
  $v = $f['compute']($idx);
  $f['coeffs'] = array_merge($f['coeffs'], [$v]);
};
  return $f['coeffs'][$n];
};
  function one() {
  return newFps(function($i) {
  if ($i == 0) {
  return 1.0;
}
  return 0.0;
});
};
  function add($a, $b) {
  return newFps(function($n) use ($a, $b) {
  return mochi_extract($a, $n) + mochi_extract($b, $n);
});
};
  function sub($a, $b) {
  return newFps(function($n) use ($a, $b) {
  return mochi_extract($a, $n) - mochi_extract($b, $n);
});
};
  function mul($a, $b) {
  return newFps(function($n) use ($a, $b) {
  $s = 0.0;
  $k = 0;
  while ($k <= $n) {
  $s = $s + mochi_extract($a, $k) * mochi_extract($b, $n - $k);
  $k = $k + 1;
};
  return $s;
});
};
  function div($a, $b) {
  $q = newFps(function($n) use ($a, $b, $q) {
  return 0.0;
});
  $q['compute'] = function($n) use ($a, $b, $q) {
  $b0 = mochi_extract($b, 0);
  if ($b0 == 0.0) {
  return (0.0 / 0.0);
}
  $s = mochi_extract($a, $n);
  $k = 1;
  while ($k <= $n) {
  $s = $s - mochi_extract($b, $k) * mochi_extract($q, $n - $k);
  $k = $k + 1;
};
  return $s / $b0;
};
  return $q;
};
  function differentiate($a) {
  return newFps(function($n) use ($a) {
  return (floatval(($n + 1))) * mochi_extract($a, $n + 1);
});
};
  function integrate($a) {
  return newFps(function($n) use ($a) {
  if ($n == 0) {
  return 0.0;
}
  return mochi_extract($a, $n - 1) / (floatval($n));
});
};
  function sinCos() {
  $sin = newFps(function($n) use ($sin) {
  return 0.0;
});
  $cos = sub(one(), integrate($sin));
  $sin['compute'] = function($n) use ($cos, $sin) {
  if ($n == 0) {
  return 0.0;
}
  return mochi_extract($cos, $n - 1) / (floatval($n));
};
  return ['sin' => $sin, 'cos' => $cos];
};
  function floorf($x) {
  $y = intval($x);
  return floatval($y);
};
  function fmtF5($x) {
  $y = floorf($x * 100000.0 + 0.5) / 100000.0;
  $s = _str($y);
  $dot = _indexof($s, '.');
  if ($dot == 0 - 1) {
  $s = $s . '.00000';
} else {
  $decs = strlen($s) - $dot - 1;
  if ($decs > 5) {
  $s = substr($s, 0, $dot + 6 - 0);
} else {
  while ($decs < 5) {
  $s = $s . '0';
  $decs = $decs + 1;
};
};
}
  return $s;
};
  function padFloat5($x, $width) {
  $s = fmtF5($x);
  while (strlen($s) < $width) {
  $s = ' ' . $s;
};
  return $s;
};
  function partialSeries($f) {
  $out = '';
  $i = 0;
  while ($i < 6) {
  $out = $out . ' ' . padFloat5(mochi_extract($f, $i), 8) . ' ';
  $i = $i + 1;
};
  return $out;
};
  function main() {
  $p = sinCos();
  echo rtrim('sin:' . partialSeries($p['sin'])), PHP_EOL;
  echo rtrim('cos:' . partialSeries($p['cos'])), PHP_EOL;
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

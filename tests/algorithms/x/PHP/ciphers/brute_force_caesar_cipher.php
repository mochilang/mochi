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
$LETTERS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
function index_of($s, $ch) {
  global $LETTERS;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return 0 - 1;
}
function decrypt($message) {
  global $LETTERS;
  for ($key = 0; $key < strlen($LETTERS); $key++) {
  $translated = '';
  for ($i = 0; $i < strlen($message); $i++) {
  $symbol = substr($message, $i, $i + 1 - $i);
  $idx = index_of($LETTERS, $symbol);
  if ($idx != 0 - 1) {
  $num = $idx - $key;
  if ($num < 0) {
  $num = $num + strlen($LETTERS);
};
  $translated = $translated . substr($LETTERS, $num, $num + 1 - $num);
} else {
  $translated = $translated . $symbol;
}
};
  echo rtrim('Decryption using Key #' . _str($key) . ': ' . $translated), PHP_EOL;
};
}
decrypt('TMDETUX PMDVU');

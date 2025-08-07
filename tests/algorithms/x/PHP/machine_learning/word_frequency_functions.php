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
function _append($arr, $x) {
    $arr[] = $x;
    return $arr;
}
$__start_mem = memory_get_usage();
$__start = _now();
  $LOWER = 'abcdefghijklmnopqrstuvwxyz';
  $UPPER = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $PUNCT = '!"#$%&\'()*+,-./:;<=>?@[\\]^_{|}~';
  function to_lowercase($s) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  $res = '';
  $i = 0;
  while ($i < strlen($s)) {
  $c = substr($s, $i, $i + 1 - $i);
  $j = 0;
  $found = false;
  while ($j < strlen($UPPER)) {
  if ($c == substr($UPPER, $j, $j + 1 - $j)) {
  $res = $res . substr($LOWER, $j, $j + 1 - $j);
  $found = true;
  break;
}
  $j = $j + 1;
};
  if (!$found) {
  $res = $res . $c;
}
  $i = $i + 1;
};
  return $res;
};
  function is_punct($c) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  $i = 0;
  while ($i < strlen($PUNCT)) {
  if ($c == substr($PUNCT, $i, $i + 1 - $i)) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function clean_text($text, $keep_newlines) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  $lower = to_lowercase($text);
  $res = '';
  $i = 0;
  while ($i < strlen($lower)) {
  $ch = substr($lower, $i, $i + 1 - $i);
  if (is_punct($ch)) {
} else {
  if ($ch == '
') {
  if ($keep_newlines) {
  $res = $res . '
';
};
} else {
  $res = $res . $ch;
};
}
  $i = $i + 1;
};
  return $res;
};
  function split($s, $sep) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  $res = [];
  $current = '';
  $i = 0;
  while ($i < strlen($s)) {
  $ch = substr($s, $i, $i + 1 - $i);
  if ($ch == $sep) {
  $res = _append($res, $current);
  $current = '';
} else {
  $current = $current . $ch;
}
  $i = $i + 1;
};
  $res = _append($res, $current);
  return $res;
};
  function contains($s, $sub) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  $n = strlen($s);
  $m = strlen($sub);
  if ($m == 0) {
  return true;
}
  $i = 0;
  while ($i <= $n - $m) {
  $j = 0;
  $is_match = true;
  while ($j < $m) {
  if (substr($s, $i + $j, $i + $j + 1 - ($i + $j)) != substr($sub, $j, $j + 1 - $j)) {
  $is_match = false;
  break;
}
  $j = $j + 1;
};
  if ($is_match) {
  return true;
}
  $i = $i + 1;
};
  return false;
};
  function mochi_floor($x) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  $i = intval($x);
  if ((floatval($i)) > $x) {
  $i = $i - 1;
}
  return floatval($i);
};
  function round3($x) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  return mochi_floor($x * 1000.0 + 0.5) / 1000.0;
};
  function ln($x) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  $t = ($x - 1.0) / ($x + 1.0);
  $term = $t;
  $sum = 0.0;
  $k = 1;
  while ($k <= 99) {
  $sum = $sum + $term / (floatval($k));
  $term = $term * $t * $t;
  $k = $k + 2;
};
  return 2.0 * $sum;
};
  function log10($x) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  return ln($x) / ln(10.0);
};
  function term_frequency($term, $document) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  $clean = clean_text($document, false);
  $tokens = explode(' ', $clean);
  $t = to_lowercase($term);
  $count = 0;
  $i = 0;
  while ($i < count($tokens)) {
  if ($tokens[$i] != '' && $tokens[$i] == $t) {
  $count = $count + 1;
}
  $i = $i + 1;
};
  return $count;
};
  function document_frequency($term, $corpus) {
  global $LOWER, $UPPER, $PUNCT, $idf_val;
  $clean = clean_text($corpus, true);
  $docs = explode('
', $clean);
  $t = to_lowercase($term);
  $matches = 0;
  $i = 0;
  while ($i < count($docs)) {
  if (contains($docs[$i], $t)) {
  $matches = $matches + 1;
}
  $i = $i + 1;
};
  return [$matches, count($docs)];
};
  function inverse_document_frequency($df, $n, $smoothing) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  if ($smoothing) {
  if ($n == 0) {
  $panic('log10(0) is undefined.');
};
  $ratio = (floatval($n)) / (1.0 + (floatval($df)));
  $l = log10($ratio);
  $result = round3(1.0 + $l);
  echo rtrim(json_encode($result, 1344)), PHP_EOL;
  return $result;
}
  if ($df == 0) {
  $panic('df must be > 0');
}
  if ($n == 0) {
  $panic('log10(0) is undefined.');
}
  $ratio = (floatval($n)) / (floatval($df));
  $l = log10($ratio);
  $result = round3($l);
  echo rtrim(json_encode($result, 1344)), PHP_EOL;
  return $result;
};
  function tf_idf($tf, $idf) {
  global $LOWER, $UPPER, $PUNCT, $corpus, $idf_val;
  $prod = (floatval($tf)) * $idf;
  $result = round3($prod);
  echo rtrim(json_encode($result, 1344)), PHP_EOL;
  return $result;
};
  echo rtrim(json_encode(term_frequency('to', 'To be, or not to be'), 1344)), PHP_EOL;
  $corpus = 'This is the first document in the corpus.
ThIs is the second document in the corpus.
THIS is the third document in the corpus.';
  echo rtrim(_str(document_frequency('first', $corpus))), PHP_EOL;
  $idf_val = inverse_document_frequency(1, 3, false);
  tf_idf(2, $idf_val);
$__end = _now();
$__end_mem = memory_get_peak_usage();
$__duration = max(1, intdiv($__end - $__start, 1000));
$__mem_diff = max(0, $__end_mem - $__start_mem);
$__bench = ["duration_us" => $__duration, "memory_bytes" => $__mem_diff, "name" => "main"];
$__j = json_encode($__bench, 128);
$__j = str_replace("    ", "  ", $__j);
echo $__j, PHP_EOL;

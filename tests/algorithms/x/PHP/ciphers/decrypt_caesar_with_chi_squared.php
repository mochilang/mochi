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
function default_alphabet() {
  global $r1, $r2, $r3;
  return ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'];
}
function default_frequencies() {
  global $r1, $r2, $r3;
  return ['a' => 0.08497, 'b' => 0.01492, 'c' => 0.02202, 'd' => 0.04253, 'e' => 0.11162, 'f' => 0.02228, 'g' => 0.02015, 'h' => 0.06094, 'i' => 0.07546, 'j' => 0.00153, 'k' => 0.01292, 'l' => 0.04025, 'm' => 0.02406, 'n' => 0.06749, 'o' => 0.07507, 'p' => 0.01929, 'q' => 0.00095, 'r' => 0.07587, 's' => 0.06327, 't' => 0.09356, 'u' => 0.02758, 'v' => 0.00978, 'w' => 0.0256, 'x' => 0.0015, 'y' => 0.01994, 'z' => 0.00077];
}
function index_of($xs, $ch) {
  global $r1, $r2, $r3;
  $i = 0;
  while ($i < count($xs)) {
  if ($xs[$i] == $ch) {
  return $i;
}
  $i = $i + 1;
};
  return -1;
}
function count_char($s, $ch) {
  global $r1, $r2, $r3;
  $count = 0;
  $i = 0;
  while ($i < strlen($s)) {
  if (substr($s, $i, $i + 1 - $i) == $ch) {
  $count = $count + 1;
}
  $i = $i + 1;
};
  return $count;
}
function decrypt_caesar_with_chi_squared($ciphertext, $cipher_alphabet, $frequencies_dict, $case_sensitive) {
  global $r1, $r2, $r3;
  $alphabet_letters = $cipher_alphabet;
  if (count($alphabet_letters) == 0) {
  $alphabet_letters = default_alphabet();
}
  $frequencies = $frequencies_dict;
  if (count($frequencies) == 0) {
  $frequencies = default_frequencies();
}
  if (!$case_sensitive) {
  $ciphertext = strtolower($ciphertext);
}
  $best_shift = 0;
  $best_chi = 0.0;
  $best_text = '';
  $shift = 0;
  while ($shift < count($alphabet_letters)) {
  $decrypted = '';
  $i = 0;
  while ($i < strlen($ciphertext)) {
  $ch = substr($ciphertext, $i, $i + 1 - $i);
  $idx = index_of($alphabet_letters, strtolower($ch));
  if ($idx >= 0) {
  $m = count($alphabet_letters);
  $new_idx = ($idx - $shift) % $m;
  if ($new_idx < 0) {
  $new_idx = $new_idx + $m;
};
  $new_char = $alphabet_letters[$new_idx];
  if ($case_sensitive && $ch != strtolower($ch)) {
  $decrypted = $decrypted . strtoupper($new_char);
} else {
  $decrypted = $decrypted . $new_char;
};
} else {
  $decrypted = $decrypted . $ch;
}
  $i = $i + 1;
};
  $chi = 0.0;
  $lowered = ($case_sensitive ? strtolower($decrypted) : $decrypted);
  $j = 0;
  while ($j < count($alphabet_letters)) {
  $letter = $alphabet_letters[$j];
  $occ = count_char($lowered, $letter);
  if ($occ > 0) {
  $occf = floatval($occ);
  $expected = $frequencies[$letter] * $occf;
  $diff = $occf - $expected;
  $chi = $chi + (($diff * $diff) / $expected) * $occf;
}
  $j = $j + 1;
};
  if ($shift == 0 || $chi < $best_chi) {
  $best_shift = $shift;
  $best_chi = $chi;
  $best_text = $decrypted;
}
  $shift = $shift + 1;
};
  return ['shift' => $best_shift, 'chi' => $best_chi, 'decoded' => $best_text];
}
$r1 = decrypt_caesar_with_chi_squared('dof pz aol jhlzhy jpwoly zv wvwbshy? pa pz avv lhzf av jyhjr!', [], [], false);
echo rtrim(_str($r1['shift']) . ', ' . _str($r1['chi']) . ', ' . $r1['decoded']), PHP_EOL;
$r2 = decrypt_caesar_with_chi_squared('crybd cdbsxq', [], [], false);
echo rtrim(_str($r2['shift']) . ', ' . _str($r2['chi']) . ', ' . $r2['decoded']), PHP_EOL;
$r3 = decrypt_caesar_with_chi_squared('Crybd Cdbsxq', [], [], true);
echo rtrim(_str($r3['shift']) . ', ' . _str($r3['chi']) . ', ' . $r3['decoded']), PHP_EOL;

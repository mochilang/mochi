<?php
function add($a, $b) {
  global $mul, $neg, $inv, $conj, $cstr;
  return ["re" => $a["re"] + $b["re"], "im" => $a["im"] + $b["im"]];
}
function mul($a, $b) {
  global $add, $neg, $inv, $conj, $cstr;
  return ["re" => $a["re"] * $b["re"] - $a["im"] * $b["im"], "im" => $a["re"] * $b["im"] + $a["im"] * $b["re"]];
}
function neg($a) {
  global $add, $mul, $inv, $conj, $cstr, $b;
  return ["re" => -$a["re"], "im" => -$a["im"]];
}
function inv($a) {
  global $add, $mul, $neg, $conj, $cstr, $b;
  $denom = $a["re"] * $a["re"] + $a["im"] * $a["im"];
  return ["re" => $a["re"] / $denom, "im" => -$a["im"] / $denom];
}
function conj($a) {
  global $add, $mul, $neg, $inv, $cstr, $b;
  return ["re" => $a["re"], "im" => -$a["im"]];
}
function cstr($a) {
  global $add, $mul, $neg, $inv, $conj, $b;
  $s = "(" . json_encode($a["re"], 1344);
  if ($a["im"] >= 0) {
  $s = $s . "+" . json_encode($a["im"], 1344) . "i)";
} else {
  $s = $s . json_encode($a["im"], 1344) . "i)";
}
  return $s;
}
$a = ["re" => 1.0, "im" => 1.0];
$b = ["re" => 3.14159, "im" => 1.25];
echo "a:       " . cstr($a), PHP_EOL;
echo "b:       " . cstr($b), PHP_EOL;
echo "a + b:   " . cstr(add($a, $b)), PHP_EOL;
echo "a * b:   " . cstr(mul($a, $b)), PHP_EOL;
echo "-a:      " . cstr(neg($a)), PHP_EOL;
echo "1 / a:   " . cstr(inv($a)), PHP_EOL;
echo "a̅:       " . cstr(conj($a)), PHP_EOL;

<?php
function main() {
  $a = 12;
  $b = 8;
  echo json_encode($a, 1344) . " + " . json_encode($b, 1344) . " = " . json_encode($a + $b, 1344), PHP_EOL;
  echo json_encode($a, 1344) . " - " . json_encode($b, 1344) . " = " . json_encode($a - $b, 1344), PHP_EOL;
  echo json_encode($a, 1344) . " * " . json_encode($b, 1344) . " = " . json_encode($a * $b, 1344), PHP_EOL;
  echo json_encode($a, 1344) . " / " . json_encode($b, 1344) . " = " . json_encode(intval((intdiv($a, $b))), 1344), PHP_EOL;
  echo json_encode($a, 1344) . " % " . json_encode($b, 1344) . " = " . json_encode($a % $b, 1344), PHP_EOL;
}
main();

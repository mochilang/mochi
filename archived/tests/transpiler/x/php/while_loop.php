<?php
$i = 0;
while ($i < 3) {
  echo (is_float($i) ? json_encode($i, 1344) : $i), PHP_EOL;
  $i = $i + 1;
}

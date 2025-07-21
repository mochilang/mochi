<?php
foreach ([1, 2, 3] as $n) {
  echo (is_float($n) ? json_encode($n, 1344) : $n), PHP_EOL;
}

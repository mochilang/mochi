<?php
for ($i = 1; $i < 4; $i++) {
  echo (is_float($i) ? json_encode($i, 1344) : $i), PHP_EOL;
}

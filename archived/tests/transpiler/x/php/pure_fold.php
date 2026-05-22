<?php
function triple($x) {
  return $x * 3;
}
echo (is_float(triple(1 + 2)) ? json_encode(triple(1 + 2), 1344) : triple(1 + 2)), PHP_EOL;

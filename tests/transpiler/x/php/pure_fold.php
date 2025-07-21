<?php
function triple($x) {
  return $x * 3;
}
echo rtrim((is_float(triple(1 + 2)) ? sprintf("%.15f", triple(1 + 2)) : triple(1 + 2))), PHP_EOL;
?>

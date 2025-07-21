<?php
echo rtrim((is_float(("a" < "b" ? 1 : 0)) ? sprintf("%.15f", ("a" < "b" ? 1 : 0)) : ("a" < "b" ? 1 : 0))), PHP_EOL;
echo rtrim((is_float(("a" <= "a" ? 1 : 0)) ? sprintf("%.15f", ("a" <= "a" ? 1 : 0)) : ("a" <= "a" ? 1 : 0))), PHP_EOL;
echo rtrim((is_float(("b" > "a" ? 1 : 0)) ? sprintf("%.15f", ("b" > "a" ? 1 : 0)) : ("b" > "a" ? 1 : 0))), PHP_EOL;
echo rtrim((is_float(("b" >= "b" ? 1 : 0)) ? sprintf("%.15f", ("b" >= "b" ? 1 : 0)) : ("b" >= "b" ? 1 : 0))), PHP_EOL;
?>

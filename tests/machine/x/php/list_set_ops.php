<?php
var_dump([1, 2] union [2, 3]);
var_dump([1, 2, 3] except [2]);
var_dump([1, 2, 3] intersect [2, 4]);
var_dump(count([1, 2] union [2, 3]));

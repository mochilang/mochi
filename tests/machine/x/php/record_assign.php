<?php
class Counter {
    public $n;
    public function __construct($fields = []) {
        $this->n = $fields['n'] ?? null;
    }
}
function inc($c) {
    $c->n = $c->n + 1;
}
$c = new Counter(['n' => 0]);
inc($c);
var_dump($c->n);

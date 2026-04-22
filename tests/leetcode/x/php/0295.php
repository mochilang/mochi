<?php
$lines = array_values(array_filter(array_map('trim', file('php://stdin')), 'strlen'));
if (count($lines) === 0) {
    exit(0);
}

class MedianFinder {
    public array $data = [];

    public function addNum(int $num): void {
        $lo = 0;
        $hi = count($this->data);
        while ($lo < $hi) {
            $mid = intdiv($lo + $hi, 2);
            if ($this->data[$mid] < $num) {
                $lo = $mid + 1;
            } else {
                $hi = $mid;
            }
        }
        array_splice($this->data, $lo, 0, [$num]);
    }

    public function findMedian(): float {
        $n = count($this->data);
        if ($n % 2 === 1) {
            return (float)$this->data[intdiv($n, 2)];
        }
        return ($this->data[intdiv($n, 2) - 1] + $this->data[intdiv($n, 2)]) / 2.0;
    }
}

$t = intval($lines[0]);
$idx = 1;
$blocks = [];
for ($tc = 0; $tc < $t; $tc++) {
    $m = intval($lines[$idx++]);
    $mf = new MedianFinder();
    $out = [];
    for ($i = 0; $i < $m; $i++) {
        $parts = preg_split('/\s+/', $lines[$idx++]);
        if ($parts[0] === 'addNum') {
            $mf->addNum(intval($parts[1]));
        } else {
            $out[] = number_format($mf->findMedian(), 1, '.', '');
        }
    }
    $blocks[] = implode("\n", $out);
}
echo implode("\n\n", $blocks);

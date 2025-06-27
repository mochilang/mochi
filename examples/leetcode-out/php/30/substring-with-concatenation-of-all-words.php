<?php
function mochi_findSubstring($s, $words) {
	if (((is_array($words) ? count($words) : strlen($words)) == 0)) {
		return [];
	}
	$wordLen = (is_array($words[0]) ? count($words[0]) : strlen($words[0]));
	$wordCount = (is_array($words) ? count($words) : strlen($words));
	$totalLen = ($wordLen * $wordCount);
	if (((is_array($s) ? count($s) : strlen($s)) < $totalLen)) {
		return [];
	}
	$freq = [];
	foreach ((is_string($words) ? str_split($words) : $words) as $w) {
		if ((is_array($freq) ? (array_key_exists($w, $freq) || in_array($w, $freq, true)) : (is_string($freq) ? strpos($freq, strval($w)) !== false : false))) {
			$freq[$w] = ((is_array($freq[$w]) && is_array(1)) ? array_merge($freq[$w], 1) : ((is_string($freq[$w]) || is_string(1)) ? ($freq[$w] . 1) : ($freq[$w] + 1)));
		} else {
			$freq[$w] = 1;
		}
	}
	$result = [];
	for ($offset = 0; $offset < $wordLen; $offset++) {
		$left = $offset;
		$count = 0;
		$seen = [];
		$j = $offset;
		while ((((is_array($j) && is_array($wordLen)) ? array_merge($j, $wordLen) : ((is_string($j) || is_string($wordLen)) ? ($j . $wordLen) : ($j + $wordLen))) <= (is_array($s) ? count($s) : strlen($s)))) {
			$word = (is_array($s) ? array_slice($s, $j, (((is_array($j) && is_array($wordLen)) ? array_merge($j, $wordLen) : ((is_string($j) || is_string($wordLen)) ? ($j . $wordLen) : ($j + $wordLen)))) - ($j)) : substr($s, $j, (((is_array($j) && is_array($wordLen)) ? array_merge($j, $wordLen) : ((is_string($j) || is_string($wordLen)) ? ($j . $wordLen) : ($j + $wordLen)))) - ($j)));
			$j = ((is_array($j) && is_array($wordLen)) ? array_merge($j, $wordLen) : ((is_string($j) || is_string($wordLen)) ? ($j . $wordLen) : ($j + $wordLen)));
			if ((is_array($freq) ? (array_key_exists($word, $freq) || in_array($word, $freq, true)) : (is_string($freq) ? strpos($freq, strval($word)) !== false : false))) {
				if ((is_array($seen) ? (array_key_exists($word, $seen) || in_array($word, $seen, true)) : (is_string($seen) ? strpos($seen, strval($word)) !== false : false))) {
					$seen[$word] = ((is_array($seen[$word]) && is_array(1)) ? array_merge($seen[$word], 1) : ((is_string($seen[$word]) || is_string(1)) ? ($seen[$word] . 1) : ($seen[$word] + 1)));
				} else {
					$seen[$word] = 1;
				}
				$count = ((is_array($count) && is_array(1)) ? array_merge($count, 1) : ((is_string($count) || is_string(1)) ? ($count . 1) : ($count + 1)));
				while (($seen[$word] > $freq[$word])) {
					$lw = (is_array($s) ? array_slice($s, $left, (((is_array($left) && is_array($wordLen)) ? array_merge($left, $wordLen) : ((is_string($left) || is_string($wordLen)) ? ($left . $wordLen) : ($left + $wordLen)))) - ($left)) : substr($s, $left, (((is_array($left) && is_array($wordLen)) ? array_merge($left, $wordLen) : ((is_string($left) || is_string($wordLen)) ? ($left . $wordLen) : ($left + $wordLen)))) - ($left)));
					$seen[$lw] = ($seen[$lw] - 1);
					$left = ((is_array($left) && is_array($wordLen)) ? array_merge($left, $wordLen) : ((is_string($left) || is_string($wordLen)) ? ($left . $wordLen) : ($left + $wordLen)));
					$count = ($count - 1);
				}
				if (($count == $wordCount)) {
					$result = ((is_array($result) && is_array([$left])) ? array_merge($result, [$left]) : ((is_string($result) || is_string([$left])) ? ($result . [$left]) : ($result + [$left])));
					$lw = (is_array($s) ? array_slice($s, $left, (((is_array($left) && is_array($wordLen)) ? array_merge($left, $wordLen) : ((is_string($left) || is_string($wordLen)) ? ($left . $wordLen) : ($left + $wordLen)))) - ($left)) : substr($s, $left, (((is_array($left) && is_array($wordLen)) ? array_merge($left, $wordLen) : ((is_string($left) || is_string($wordLen)) ? ($left . $wordLen) : ($left + $wordLen)))) - ($left)));
					$seen[$lw] = ($seen[$lw] - 1);
					$left = ((is_array($left) && is_array($wordLen)) ? array_merge($left, $wordLen) : ((is_string($left) || is_string($wordLen)) ? ($left . $wordLen) : ($left + $wordLen)));
					$count = ($count - 1);
				}
			} else {
				$seen = [];
				$count = 0;
				$left = $j;
			}
		}
	}
	return $result;
}

function mochi_test_example_1() {
	if (!((mochi_findSubstring("barfoothefoobarman", ["foo", "bar"]) == [0, 9]))) { throw new Exception('expect failed'); }
}

function mochi_test_example_2() {
	if (!((mochi_findSubstring("wordgoodgoodgoodbestword", ["word", "good", "best", "word"]) == []))) { throw new Exception('expect failed'); }
}

function mochi_test_example_3() {
	if (!((mochi_findSubstring("barfoofoobarthefoobarman", ["bar", "foo", "the"]) == [6, 9, 12]))) { throw new Exception('expect failed'); }
}

mochi_test_example_1();
mochi_test_example_2();
mochi_test_example_3();

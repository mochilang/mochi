<?php
function isMatch($s, $p) {
	$m = count($s);
	$n = count($p);
	$memo = [];
	return dfs(0, 0);
}


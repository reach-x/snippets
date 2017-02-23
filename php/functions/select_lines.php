<?php
$file = fopen('php://stdin', 'r');

$wanted_lines = array(
	2,
	4,
	5,
	8,
);
$current_line = 0;

while ($line = fgets($file)) {
	$current_line++;
	if (in_array($current_line, $wanted_lines, TRUE)) {
		echo $line;
	}
}

fclose($file);

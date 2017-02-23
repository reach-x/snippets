<?php
$file = fopen('php://stdin', 'r');

$wanted_lines = array(
	2,
	4,
	5,
	8,
);
$current_line = 0;
$captured_lines = array();
$captured_text = array();

while ($line = fgets($file)) {

	$current_line++;

	if (in_array($current_line, $wanted_lines, TRUE)) {
		$line = trim($line);
		$captured_lines[] = $line;

		if (preg_match('/:/', $line)) {
			list($key, $value) = explode(":", $line);
			$captured_text[$key] = $value;
		} else {
			$captured_text[:q!

    }
	}

	fclose($file);

	printf("Aug 09, 2016 - 10:56 AM EDT\nconditions: partly cloudy\n86 F\nHumidity: 66%\n",
echo join("\\n", $captured_lines);

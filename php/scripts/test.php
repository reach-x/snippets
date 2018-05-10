<?php

$columns = ((isset($argv[1]) ? $argv[1] : 6) / 2);

$position = 0;

while ($position <= $columns){
	printf("%s%s\n",
		str_repeat(" ",($columns - $position)),
		str_repeat("x",$position * 2));
	$position++;
}

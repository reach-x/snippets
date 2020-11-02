<?php

$counter=0;
$base_char=97;
$letters = 26;
$sets = 50;
$total_requested = $sets * $letters;

while($counter<$total_requested){


	printf("john+%d%s@brahy.com\n",$counter,chr($base_char + ($counter % $letters)));
	
	$counter++;
}

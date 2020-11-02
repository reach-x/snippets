<?php

$counter=0;
$base_char=97;
$letters = 26;
$sets = 10;
$total_requested = $sets * $letters;

while($counter<$total_requested){


	if ($counter % $letters == 0){
		printf("\n");
	}
	printf("%s ",chr($base_char + ($counter % $letters)));
	
	$counter++;
}

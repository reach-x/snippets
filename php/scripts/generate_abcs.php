<?php

$counter=0;
$base_char=97;


while($counter<100){

	printf("%s ",chr($base_char + ($counter % 3)));

	if ($counter % 3 == 0){
		printf("\n");
	}
	
	//printf("%s: %s\n",chr($base_char + $counter),($base_char + $counter));
	$counter++;
}

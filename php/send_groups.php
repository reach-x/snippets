<?php

$counter = 0;
$send_groups = array('Group A','Group B', 'Group C', 'Group D');

while ($counter < 100){
	printf("%s\n",$send_groups[$counter % 4]);
	$counter++;
}




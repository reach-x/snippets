<?php

$month = 1;
$day = 1;

while ($month <= 12){
	while ($day <= 31){
		printf("CREATE TABLE birthday_export_%02d%02d as SELECT * FROM exportable_owner_post_records WHERE birthday='%02d/%02d';\n",$month,$day,$month,$day);
		$day++;
	}
	$day=1;
	$month++;
}

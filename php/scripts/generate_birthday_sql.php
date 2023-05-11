<?php

$month = 1;
$day = 1;

while ($month <= 12){
	while ($day <= 31){
		printf("UPDATE owner_post_dobs SET birthday='%02d/%02d' WHERE full_content like '%%%02d-%02d%%' AND birthday IS NULL;\n",$month,$day,$month,$day);
		printf("UPDATE owner_post_dobs SET birthday='%02d/%02d' WHERE full_content like '%%%02d/%02d%%' AND birthday IS NULL;\n",$month,$day,$month,$day);
		$day++;
	}
	$day=1;
	$month++;
}

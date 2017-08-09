<?php


$is_valid = (strtotime("today - 14 days") < strtotime($argv[1]));

if ($is_valid){
	printf("is_valid: true\n");
} else {
	printf("is_valid: false\n");
}


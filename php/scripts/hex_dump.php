<?php

$letters = str_split($argv[1]);

foreach ($letters as $arguement_variable){

	printf("%s: %d\n",$arguement_variable,ord($arguement_variable));
}

<?php

printf("now: %d\n",strtotime("now"));

$scheduled_time = '1415652497';
//$scheduled_time = '2014-10-01 12:00:00.00';
$rate_limit = "1";
$next_time =    $scheduled_time . " +" . $rate_limit . " seconds";

printf("%s\n",$next_time);


printf("%s\n",date('Y-m-d H:i:s', strtotime($next_time)));


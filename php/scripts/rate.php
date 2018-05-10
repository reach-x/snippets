<?php

$counter = 500;
$start_time = microtime(true);
sleep(2);
$rate = round(( $counter > 0 ? ( 1000 / ( microtime(TRUE) - $start_time ) ) : 0 ), 2);

printf("rate is: %s\n", $rate);


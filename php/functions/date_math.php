<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 3/13/17
 * Time: 11:31
 */

$created_at = strtotime($argv[1]);
$max_retry_period = 24 * 60 * 60 * 7;
$now = strtotime("now");

if ($created_at + $max_retry_period > $now) {
	printf(">\n");
} else {
	printf("<\n");
}

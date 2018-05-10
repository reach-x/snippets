<?php


date_default_timezone_set('Europe/London');

$datetime = new DateTime('2008-08-03 12:35:23');
printf("Europe/London: %s\n",$datetime->format('Y-m-d H:i:s'));

$la_time = new DateTimeZone('America/Los_Angeles');
$datetime->setTimezone($la_time);
printf("America/Los_Angeles: %s\n",$datetime->format('Y-m-d H:i:s'));


<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 3/9/17
 * Time: 12:49
 */
//FILTER_SCHEDULE

if ($argc == 2) {
	$current_time = $argv[1];
	$current_day = date('l');
} elseif ($argc == 3) {
	$current_time = $argv[1];
	$current_day = $argv[2];
} else {
	$current_day = date('l');
	$current_time = date("Hi");
}

$criteria = file_get_contents("../json/schedule.json");

$schedule = json_decode($criteria, TRUE);
$current_day = date('l');
$open_time = str_replace(":", "", $schedule[$current_day]['open']);
$close_time = str_replace(":", "", $schedule[$current_day]['close']);
$is_filtered = ($current_time < $open_time || $current_time > $close_time) ? TRUE : FALSE;

print_r([
	'current_time' => $current_time,
	'current_day' => $current_day,
	'is_filtered' => $is_filtered ? "TRUE" : "FALSE",
	'schedule' => $schedule[$current_day],
]);
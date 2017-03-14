<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 3/10/17
 * Time: 09:28
 */

$schedule_json = file_get_contents("../json/schedule.json");

$schedule = json_decode($schedule_json);

if (isset($schedule->whatever)){
	print "isset\n";
} else {
	print "!isset\n";
}

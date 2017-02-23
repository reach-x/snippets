<?php
date_default_timezone_set("US/Pacific");

$unsubscribe_email = "no@sense.com";

$filename = sprintf("/tmp/unsubscribe-%s.log", date("Y-m-d"));
file_put_contents($filename, sprintf("%s\n", $unsubscribe_email), FILE_APPEND);





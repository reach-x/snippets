<?php
$unix_timestamp=$argv[1];

$date_array = array(
  'month' => date("m", ($unix_timestamp)),
  'day' => date("d", ($unix_timestamp)),
  'year' => date("Y", ($unix_timestamp)),
  'hour' => date("H", ($unix_timestamp)),
  'min' => date("i", ($unix_timestamp)),
  'ampm' => date("A", ($unix_timestamp)),
);

print_r($date_array);
printf("\n");

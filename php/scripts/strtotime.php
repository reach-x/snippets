<?php
$drop_date=$argv[1];

$date_array = array(
  'month' => date("m", strtotime($drop_date)),
  'day' => date("d", strtotime($drop_date)),
  'year' => date("Y", strtotime($drop_date)),
  'hour' => date("H", strtotime($drop_date)),
  'min' => date("i", strtotime($drop_date)),
  'ampm' => date("A", strtotime($drop_date)),
);

print_r($date_array);
print_r(array('unixtimestamp' => strtotime($drop_date)));
printf("\n");

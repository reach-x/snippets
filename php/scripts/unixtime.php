<?php
$timestamp =$argv[1];

$date_array = array(
  'month' => date("m", $timestamp),
  'day' => date("d", $timestamp),
  'year' => date("Y", $timestamp),
  'hour' => date("H", $timestamp),
  'min' => date("i", $timestamp),
  'ampm' => date("A", $timestamp),
);

print_r($date_array);
print_r(array('unixtimestamp' => $timestamp));
printf("\n");

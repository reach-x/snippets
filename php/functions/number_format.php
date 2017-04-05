<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 4/4/17
 * Time: 14:19
 */

$number = isset($argv[1]) ? $argv[1] : 1000000;


printf("original: %s\tnumber_format: %s\n", $number, number_format($number));
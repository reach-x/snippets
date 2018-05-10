<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 4/6/18
 * Time: 13:31
 */


$opener_json = file_get_contents($argv[1]);
$opener_json = substr($opener_json, 1, strlen($opener_json) - 2);
$opener_array = json_decode($opener_json, TRUE);
print_r($opener_array);
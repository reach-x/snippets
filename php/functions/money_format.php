<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 4/4/17
 * Time: 14:19
 */

$money = isset($argv[1])  ? $argv[1] : 1000000;


printf("original: %s\tmoney_format: %s\n",$money,money_format("%i",$money));
<?php


$deal_key = "ASDF-asdf-~1234'!@#$$#@!$#!%$$^%$&^%$&*%$*&^*^&*";
$deal_key = preg_replace("/[^_\-a-zA-Z0-9]+/", "", $deal_key);

printf("%s\n",$deal_key);



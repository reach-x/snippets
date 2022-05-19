<?php

$string = "http://www.dreamjobs.mobi?hid={transaction_id}";
$url = parse_url($string);

print_r($url);


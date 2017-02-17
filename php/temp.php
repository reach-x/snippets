<?php


$url = "http://www.somesite.com/somepage.php?&validation=3545&validation=1431";
$parsed_url = parse_url($url);
$query = $parsed_url['query'];
parse_str($query,$parts);

print_r(array('parts'=>$parts));




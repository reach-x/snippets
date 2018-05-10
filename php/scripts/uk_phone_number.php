<?php

$content = $argv[1];


$phone_number = str_split($content);

if ($phone_number[0] != "0") {
    printf("0%d", $content);
} else {
    printf("%d\n",$content);
}



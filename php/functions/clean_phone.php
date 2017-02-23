<?php

$phone_number = "ASDF!@#$43211234ASD";
$clean_phone_number = preg_replace("/[^0-9]/", "", $phone_number);
printf("phone: %s\n", $clean_phone_number);

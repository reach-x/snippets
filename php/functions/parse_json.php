<?php

$contents = file_get_contents($argv[1]);
print_r(array('parsed' => json_decode($contents)));

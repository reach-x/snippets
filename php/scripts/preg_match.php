#!/usr/local/bin/php
<?php

$matches = array();
print_r(preg_match($argv[1],$argv[2],$matches));
print_r($matches);


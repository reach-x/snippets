<?php

print strtoupper(hash($argv[1],$argv[2],FALSE) . "\n");

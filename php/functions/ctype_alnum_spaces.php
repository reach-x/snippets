<?php
$strings = array('KjgWZC', 'asdf fdsa', 'arf12 asdf');
foreach ($strings as $testcase) {
    $test_string = preg_replace('/\s+/', '', $testcase);

    if (ctype_alpha($test_string)) {
        echo "The string $testcase consists of all letters.\n";
    } else {
        echo "The string $testcase does not consist of all letters.\n";
    }
}

<?php
$strings = array('KjgWZC', 'asdf fdsa', 'arf12 asdf');
foreach ($strings as $testcase) {
    if (ctype_alpha($testcase)) {
        echo "The string $testcase consists of all letters.\n";
    } else {
        echo "The string $testcase does not consist of all letters.\n";
    }
}

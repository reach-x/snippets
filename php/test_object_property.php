<?php


$object = new stdClass();
$object->asdf = "word";
$object  = FALSE;

print_r(array(
	"isset-exists" => isset($object->asdf),
	"isset-not-exists" => isset($object->fdsa),
));

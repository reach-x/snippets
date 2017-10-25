<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 8/28/17
 * Time: 14:25
 */


$a = array(
	'test' => 1,
	'hello' => NULL,
	'pie' => array('a' => 'apple'),
	'space' => ' '
);

var_dump(isset($a['test']));            // TRUE
var_dump(isset($a['foo']));             // FALSE
var_dump(isset($a['hello']));           // FALSE
var_dump(isset($a['nonexistant']));


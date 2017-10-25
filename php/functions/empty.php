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
	'space' => ' ',
	'nospace' => '',
);

var_dump(empty($a['test']));            // TRUE
var_dump(empty($a['foo']));             // FALSE
var_dump(empty($a['hello']));           // FALSE
var_dump(empty($a['nonexistant']));
var_dump(empty($a['space']));
var_dump(empty($a['nospace']));


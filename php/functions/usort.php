<?php
function cmp($a, $b)
{
//	print_r(array(
//		'a' => print_r(array('a' => $a)),
//		'b' => print_r(array('b' => $b)),
//	));
	return $a['order_id'] <=> $b['order_id'];
}

$a = array(
	"4" => array("order_id" => 5),
	"1" => array("order_id" => 4),
	"2" => array("order_id" => 3),
	"45" => array("order_id" => 2),
	"5" => array("order_id" => 1),
);

usort($a, function($a, $b)
{
	//	print_r(array(
	//		'a' => print_r(array('a' => $a)),
	//		'b' => print_r(array('b' => $b)),
	//	));
	return $a['order_id'] <=> $b['order_id'];
});

foreach ($a as $key => $value) {
	print_r(array('key' => $key, 'value' => $value));
}
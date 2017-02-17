<?php

$weighted_buyers = [
	10 => "10 - asdf",
	9 => "9 - asdf",
	8 => "8 - asdf",
	7 => "7 - asdf",
	6 => "6 - asdf",
	5 => "5 - asdf",
	4 => "4 - asdf",
	3 => "3 - asdf",
	2 => "2 - asdf",
	1 => "1 - asdf",
	0 => "0 - asdf",
];

ksort($weighted_buyers);

while($buyer = array_pop($weighted_buyers)){

	print_r($buyer);
}


<?php

print_r(array(
	'is_valid_phone' => is_valid_phone("4242374565")
));


function is_valid_phone($phone_number) : bool
{
	return preg_match('/^\+?1?[-.\s]?(\([2-9][0-8][0-9]\)|[2-9][0-8][0-9])[-.\s]?[2-9][0-9]{2}[-.\s]?[0-9]{4}$/', $phone_number);
}

<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 3/14/17
 * Time: 11:46
 */


$test_backtrace = new test_backtrace();

printf("test_function(%s)\n", $test_backtrace->test_function("asdf"));


class test_backtrace {

	function test_function($parameters) {

//		print_r(['debug_backtrace()' => debug_backtrace()]);

		$trace = debug_backtrace();
		$caller = $trace[0];

		echo "Called by {$caller['function']}";

		if (isset($caller['class'])) {
			echo " in {$caller['class']}";
		}

		return $parameters;
	}
}

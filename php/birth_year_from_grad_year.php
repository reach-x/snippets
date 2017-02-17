<?php

date_default_timezone_set("US/Pacific");

$grad = $argv[1];

		if ($grad < 1900) {
			$grad += 1900;
		}

printf("%s\n",date("m/d/Y", strtotime(sprintf("%d - 18 years", $grad))));




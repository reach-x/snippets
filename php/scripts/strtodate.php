<?php

        $start_date = date("Y-m-d", strtotime("now -2 weeks"));
	        $end_date = date("Y-m-d", strtotime("now"));

		printf("%s -> %s\n",$start_date, $end_date);

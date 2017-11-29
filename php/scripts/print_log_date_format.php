<?php
date_default_timezone_set("US/Pacific");


printf("date: %s\n", date("[D M d H:i:s Y]", strtotime("now - 24 hours")));


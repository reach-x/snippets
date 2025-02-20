<?php

print_r(new DatePeriod(new DateTime("2024-05-01 00:00:00"), new DateInterval('PT1H'), new DateTime("now")));

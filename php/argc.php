<?php

printf("argc == %d\n",$argc);

foreach ($argv as $index => $argument){
	printf("%d: %s\n",$index,$argument);
}



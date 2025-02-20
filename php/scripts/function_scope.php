<?php

// Q: does a continue statement have a continue context from a function call within a foreach?
// A: no, it doesn't work. 
// Message: PHP Fatal error:  Cannot break/continue 1 level 

foreach(array(1,2,3) as $index){

	test_method($index);
}


function test_method($index){

	if ($index == 2){
	
//		continue;
		
	} else {
	
		printf("%d\n", $index);

	}
}


<?php


     $key = "sid";
     $advertiser_id = 1;
     $values = array(1,2,3,4,5);
     $desired_array = array();

//     foreach($values as $value){
//         $desired_array[] = array($key => $value);
//     }

    $desired_array = array_map(function($value) use($key){
	return array($key=>$value);
    },$values);

     print_r($desired_array);


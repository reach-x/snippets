
<?php

$test = new test();

print_r($test->get_clickpath_sequence(1));



class test {

    function get_clickpath_sequence ($clickpath_id) {
        return $this->execute(132, array( 'CLICKPATH_ID' => $clickpath_id ));
    }


    function execute ($query_id, $parameters = array()) {

        $sql = "SELECT * FROM clickpaths where clickpath_id=%%CLICKPATH_ID%%";

        if (count($parameters) > 0) {
            foreach ($parameters as $key => $value) {
                $key_pattern = sprintf("%%%%%s%%%%", strtoupper($key));
                $sql = str_ireplace($key_pattern, $value, $sql);
            }
        }

   	printf("SQL: %s\n",$sql); 
    }
}

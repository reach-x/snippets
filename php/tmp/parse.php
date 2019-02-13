<?php
/**
 * Created by PhpStorm.
 * User: jbrahy
 * Date: 11/30/18
 * Time: 10:07
 */

//libxml_use_internal_errors(TRUE);


$file_contents = sprintf("%s</CounterNotification> </DDCCommonData>",file_get_contents("temp.xml"));

$xml = simplexml_load_string($file_contents) or die("Error: Cannot create object");

//$xml = simplexml_load_file("temp.xml") or die("Error: Cannot create object");


foreach ($xml->children() as $row) {
	$MachineModel = $row->MachineModel;
	$SerialNumber = $row->SerialNumber;

	$sql = "INSERT INTO counts(MachineModel ,SerialNumber) VALUES ('" . $MachineModel . "','" . $SerialNumber . "')";

	printf("SQL: %s\n", $sql);
}




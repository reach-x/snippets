<?php

include_once("/var/www/vhosts/inboxmediacorp.com/subdomains/admin/httpdocs/Includes/configure.php");

///* interface with remote server 'imbk5' */
//
//abstract class ListInterface {
//
//	// grab the available lists default sorted by "ID" in descending order
//	// function getLists($start=0, $limit=10, $orderby="ID", $order_sort="DESC");
//
//}

class imbk5 extends mysqli {

	var $db;

	public function __construct() {

		list($server_ip, $server_port) = explode(":", DB_SERVER_4);

		// connect to the server
		$this->connect($server_ip, DB_SERVER_USERNAME_4, DB_SERVER_PASSWORD_4, DB_ADMIN_DATABASE_4);

		if ($this->connect_errno > 0) {
			die (sprintf("Could not connect to imbk5: %s\n", $this->connect_error));
		}
	}

	function getLists($start = 0, $limit = 10, $orderby = "ID", $order_sort = "DESC") {

		$list_array = array();

		$sql = "
				SELECT
					`ll`.ID,
					`ll`.LID,
					`ll`.VID,
					`ll`.ClientName,
					`ll`.ListName,
					`ll`.ListCode,
					`ll`.LiveFeed
				FROM
					`LISTS` ll
				ORDER BY 
					$orderby $order_sort
				LIMIT 
					$start, $limit
			";

		// echo str_replace("\n", "<br>", $get_lists_sql);


		$result = $this->query($sql) or printf("imbk5 class SQL Error: %s\n", $sql);

		while ($gl_arr = $result->fetch_array()) {
			$list_array[] = array(
				'ID' => $gl_arr['ID'],
				'LID' => $gl_arr['LID'],
				'VID' => $gl_arr['VID'],
				'ClientName' => $gl_arr['ClientName'],
				'ListName' => $gl_arr['ListName'],
				'LiveFeed' => $gl_arr['LiveFeed']
			);
		}

		return $list_array;
	}

	function get_list_name_by_id($id) {

		$sql = "
				SELECT
					`ll`.ListName
				FROM
					`LISTS` ll
				WHERE
					`ll`.`ID` = '$id'
			";

		$result = $this->query($sql) or printf("imbk5 class SQL Error: %s\n", $sql);
		$gl_arr = $result->fetch_array();
		$list_name = $gl_arr['ListName'];

		if (empty($list_name)) {
			$list_name = "(Unknown)";
		}

		return $list_name;
	}


	function get_list_name_by_vid($vid) {

		$vid = (int) $vid;

		$sql = "
				SELECT
					`ll`.ListName
				FROM
					`LISTS` ll
				WHERE
					`ll`.`VID` = '$vid'
				LIMIT 1
			";

		$result = $this->query($sql) or printf("imbk5 class SQL Error: %s\n", $sql);

		if ($result) {

			$gl_arr = $result->fetch_array();

			return $gl_arr['ListName'];

		} else {

			return "(Unknown)";
		}
	}

	function get_list_IDS($list_name) {

		$sql = "
				SELECT
					`ll`.`ID`,
					`ll`.`LID`
				FROM
					`LISTS` `ll`
				WHERE
					`ll`.`ListName` = '$list_name'
			";

		$result = $this->query($sql) or printf("imbk5 class SQL Error: %s\n", $sql);
		$gl_ids = $result->fetch_array();

		return array(
			'AID' => $gl_ids['ID'],
			'LID' => $gl_ids['LID'],
		);
	}

	function __destruct() {
		$this->close();
	}

}

?>

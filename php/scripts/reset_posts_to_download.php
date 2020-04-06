<?php
/*

    Project: LBC
    Built by: jbrahy
    Filename: get_primary_ids.php
    Started At: 3/26/20 13:03
    Using: PhpStorm
*/

$database = new \database();
$primary_keys_at_zero = array();
$database->server = "database.pm.internal";
$mysqli = $database->connect();
$sql = "SELECT manager_list_id FROM temp.ids";
$result = $mysqli->query($sql);
$manager_lists = array();

while ($row = $result->fetch_array(MYSQLI_ASSOC)) {

	$manager_lists[] = $row['manager_list_id'];
}

foreach ($manager_lists as $manager_list_id) {

	printf("Processing: %d\n", $manager_list_id);
	$mysqli->query("UPDATE lbc.manager_posts SET status_id=6 WHERE manager_list_id = {$manager_list_id} AND manager_posts.created_at > '2020-03-25'");
	printf("Updated: %d rows\n", $mysqli->affected_rows);
}

class database {

	public $username = "root";
	public $password = "<password>";
	public $database = "<database_name>";
	public $server = "";
	public $port = 3306;

	function connect() {

		$mysqli = new \mysqli($this->server, $this->username, $this->password, $this->database, $this->port);

		if ($mysqli->connect_error) {
			die('Connect Error (' . $mysqli->connect_errno . ') ' . $mysqli->connect_error);
		}

		return $mysqli;
	}
}

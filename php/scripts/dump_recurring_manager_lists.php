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

$sql = <<<EOQ
SELECT consumer_job_id, 
       concat(manager_list,': ',manager_list_id) manager_list, 
       content
FROM lbc.consumer_jobs 
	INNER JOIN lbc.manager_lists USING(manager_list_id)
WHERE consumer_job_type_id=4 
	AND consumer_jobs.status_id=1
	-- AND manager_lists.status_id=0
-- AND manager_id NOT IN (26,47,158,186,250)
--	AND manager_list LIKE '%uk%'
EOQ;

$result = $mysqli->query($sql);

printf("CJID\tManager List\tPath\n");

while ($row = $result->fetch_array(MYSQLI_ASSOC)) {

	$consumer_job_id = $row['consumer_job_id'];
	$content = $row['content'];
	$manager_list = $row['manager_list'];
	$content_object = json_decode($content, TRUE);

	printf("%d\t%s\t%s\n", $consumer_job_id, $manager_list, $content_object['save_file_path']);
	//printf("CJID: %d Manager List: %s Path: %s\n", $consumer_job_id, $manager_list, $content_object['save_file_path']);
	//printf("%d,", $consumer_job_id);
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

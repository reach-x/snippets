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
$database->server = "db01.mypm.us";
$mysqli = $database->connect();

$sql = <<<EOQ
SELECT * 
FROM information_schema.tables 
WHERE table_schema NOT IN ('mysql','information_schema','performance_schema')
	AND tables.auto_increment IS NOT NULL
	AND table_name NOT IN ('manager_posts','owner_posts','owner_posts_data')
	AND table_rows < 1000
EOQ;

$tables_to_check = array();
$result = $mysqli->query($sql);

while ($row = $result->fetch_array(MYSQLI_ASSOC)) {
	$tables_to_check[] = array(
		'table_schema' => $row['TABLE_SCHEMA'],
		'table_name' => $row['TABLE_NAME'],
	);
}

$autoincrement_rows_check_sql = array();

foreach ($tables_to_check as $table_to_check) {

	$sql = <<<EOQ
	SELECT column_name 
	FROM information_schema.columns 
	WHERE  table_schema='{$table_to_check['table_schema']}' 
    	AND table_name='{$table_to_check['table_name']}'
    	AND extra LIKE '%auto_increment%'
EOQ;

	$result = $mysqli->query($sql);

	while ($row = $result->fetch_array(MYSQLI_ASSOC)) {

		$sql = <<<EOQ
SELECT min({$row['column_name']}) AS min_primary_key_value 
FROM {$table_to_check['table_schema']}.{$table_to_check['table_name']}
EOQ;
		$autoincrement_rows_check_sql[] = array(
			'sql' => $sql,
			'table_name' => $table_to_check['table_name'],
			'table_schema' => $table_to_check['table_schema'],
			'primary_key' => $row['column_name'],
		);
	}
}

foreach ($autoincrement_rows_check_sql as $autoincrement_row_check_sql) {

	$result = $mysqli->query($autoincrement_row_check_sql['sql']);

	if ($mysqli->errno) {
		print_r(array(
			'$autoincrement_row_check_sql' => $autoincrement_row_check_sql,
			'error' => $mysqli->error,
		));
	}

	while ($row = $result->fetch_array(MYSQLI_ASSOC)) {

		if ($row['min_primary_key_value'] == 0 && !is_null($row['min_primary_key_value'])) {
			$primary_keys_at_zero[] = array_merge($autoincrement_row_check_sql, $row);
		}
	}
}

foreach ($primary_keys_at_zero as $primary_key_at_zero) {
	//printf("TABLE: %s.%s\n", $primary_key_at_zero['table_schema'], $primary_key_at_zero['table_name']);
	printf("/usr/local/bin/mysqldump %s %s > %s-%s.sql\n", $primary_key_at_zero['table_schema'], $primary_key_at_zero['table_name'], $primary_key_at_zero['table_schema'], $primary_key_at_zero['table_name']);

}
exit;

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

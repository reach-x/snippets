<?php


class database {

	public $username = "adtrust";
	public $password = "<password>";
	public $database = "<database_name>";
	public $name = "lbc";
	public $server = 'pm-prod-db01.cjcyp4tyx7zk.us-east-1.rds.amazonaws.com';
	public $port = 3306;

	/**
	 * @param array $params
	 */
	function init(array $params) {

		foreach ($params as $key => $value) {
			$this->{$key} = $value;
		}
	}

	function connect() {

		$mysqli = new mysqli($this->server, $this->username, $this->password, $this->database, $this->port);

		if ($mysqli->connect_error) {
			print_r(array(
				'error' => $mysqli->error,
				'connect_error' => $mysqli->connect_error,
				'opts' => array(
					$this->server,
					$this->username,
					$this->password,
					$this->database,
					$this->port,
				),
			));
			die('Connect Error (' . $mysqli->connect_errno . ') ' . $mysqli->connect_error);
		}

		return $mysqli;
	}
}

$database = new database();

print_r($database->connect());





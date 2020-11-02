<?php

class database {

    //	public $username = "adtrust";
    //	public $password = "<password>";
    public $database = "<database_name>";
    public $name = "lbc";
    //	public $server = 'pm-prod-db01.cjcyp4tyx7zk.us-east-1.rds.amazonaws.com';
    public $port = 3306;
    public $server = 'cbs.database.pm.internal';
    public $username = 'adtrust';
    public $password = '<password>';
    public $mysqli;

    /**
     * @param array $params
     */
    function init( array $params ) {

        foreach ( $params as $key => $value ) {
            $this->{$key} = $value;
        }
    }

    function connect() {

        $this->mysqli = new mysqli( $this->server, $this->username, $this->password, $this->database, $this->port );

        if ( $this->mysqli->connect_error ) {
            print_r( array(
                'error'         => $this->mysqli->error,
                'connect_error' => $this->mysqli->connect_error,
                'opts'          => array(
                    $this->server,
                    $this->username,
                    $this->password,
                    $this->database,
                    $this->port,
                ),
            ) );
            die( 'Connect Error (' . $this->mysqli->connect_errno . ') ' . $this->mysqli->connect_error );
        }

        return $this->mysqli;
    }
}

$database = new database();

print_r( $database->connect() );





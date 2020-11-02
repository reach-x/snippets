<?php

$database = new database();
$mysqli = $database->connect();

$file_handle = fopen( $argv[1], "r" );

$sql = <<<EOQ
INSERT INTO ip2location_database_tmp
    (ip_from, ip_to, country_code, country_name, region_name, city_name, latitude, longitude, zip_code, time_zone, isp, domain, net_speed, idd_code, area_code, weather_station_code, weather_station_name, mcc, mnc, mobile_brand, elevation, usage_type)
VALUES
	(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);
EOQ;

while ( $line = fgetcsv( $file_handle ) ) {

    list( $ip_from, $ip_to, $country_code, $country_name, $region_name, $city_name, $latitude, $longitude, $zip_code, $time_zone, $isp, $domain, $net_speed, $idd_code, $area_code, $weather_station_code, $weather_station_name, $mcc, $mnc, $mobile_brand, $elevation, $usage_type ) = $line;
    $statement = $mysqli->prepare( $sql );

    if ( $mysqli->errno > 0 ) {
        print_r( array(
            '$mysqli->errno' => $mysqli->errno,
            '$mysqli->error' => $mysqli->error,
            '$statement->errno' => $statement->errno,
            '$statement->error' => $statement->error,
        ) );
    }

    $statement->bind_param( "iissssssssssssssssssss", $ip_from, $ip_to, $country_code, $country_name, $region_name, $city_name, $latitude, $longitude, $zip_code, $time_zone, $isp, $domain, $net_speed, $idd_code, $area_code, $weather_station_code, $weather_station_name, $mcc, $mnc, $mobile_brand, $elevation, $usage_type );
    $statement->execute();
}

$statement->close();
$mysqli->close();

class database {

    public $database = "<database_name>";
    public $port = 25060;
    public $server = 'web-system-db-do-user-1863227-0.b.db.ondigitalocean.com';
    public $username = 'doadmin';
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

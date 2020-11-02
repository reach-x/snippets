<?php

if ( !check_post_request() ) {
    die();
}

if ( isset( $_POST["email-address"] ) ) {

    $email_to = "unsubscribe@join-class-action.com";
    $email_subject = "New Unsubscribe Request";

    $phone = filter_var( $_POST["phone"], FILTER_SANITIZE_STRING );
    $email_address = filter_var( $_POST["email-address"], FILTER_VALIDATE_EMAIL );

    $transaction_id = "";
    $affiliate_id = "";

    if ( !check_string( $phone ) ) {
        die();
    }
    if ( !check_string( $email_address ) ) {
        die();
    }

    if ( !check_new_lines( $phone ) ) {
        die();
    }
    if ( !check_new_lines( $email_address ) ) {
        die();
    }

    $headers = "From: Join Class Action Unsubscribe Request Form\r\n" . "Reply-To: " . $email_address . "\r\n" . "X-Mailer: PHP/" . phpversion() . "\r\n";

    $message = "\n\nPhone: " . $phone . "\n\nEmail: " . $email_address . "\n\n";

    mail( $email_to, $email_subject, $message, $headers );

    $sql = <<<EOQ
INSERT INTO unsubscribes (phone_number, email_address, ip_address, user_agent, referer, transaction_id, affiliate_id)
VALUES
(?, ?, ?, ?, ?, ?, ?);
EOQ;

    $ip_address = new ip_address();
    $user_agent = isset( $_SERVER["HTTP_USER_AGENT"] ) ? $_SERVER["HTTP_USER_AGENT"] : "";
    $referer = isset( $_SERVER["HTTP_REFERER"] ) ? $_SERVER["HTTP_REFERER"] : "";
    $database = new database();
    $mysqli = $database->connect();
    $statement = $mysqli->prepare( $sql );
    $statement->bind_param( "sssssss", $phone, $email_address, $ip_address->get(), $user_agent, $referer, $transaction_id, $affiliate_id );
    $statement->execute();
    $statement->close();
    $mysqli->close();

    header( "Location: unsubscribe-success.html" );
}

function check_post_request() {

    if ( $_SERVER["REQUEST_METHOD"] != "POST" ) {
        return FALSE;
    }

    return TRUE;
}

function check_string( $string ) {

    $bad_strings = array(
        "content-type:",
        "mime-version:",
        "multipart/mixed",
        "Content-Transfer-Encoding",
        "bcc:",
        "cc:",
        "to:",
        "from:",
    );

    foreach ( $bad_strings as $bad_string ) {
        $bad_string = sprintf( "/%s/gi", $bad_string );

        if ( preg_match( $bad_string, strtolower( $string ) ) ) {
            return FALSE;
        }
    }

    return TRUE;
}

function check_new_lines( $string ) {

    if ( preg_match( "/(%0A|%0D|\\n+|\\r+)/i", $string ) != 0 ) {
        return FALSE;
    }

    return TRUE;
}

class database {

    public $database = "<database_name>";
    public $port = 3306;
    public $server = "127.0.0.1";
    public $username = "unsub";
    public $password = "<password>";

    function connect() {

        $mysqli = new mysqli( $this->server, $this->username, $this->password, $this->database, $this->port );

        if ( $mysqli->connect_error ) {
            print_r( array(
                "error"         => $mysqli->error,
                "connect_error" => $mysqli->connect_error,
                "opts"          => array(
                    $this->server,
                    $this->username,
                    $this->password,
                    $this->database,
                    $this->port,
                ),
            ) );
            die( "Connect Error (" . $mysqli->connect_errno . ") " . $mysqli->connect_error );
        }

        return $mysqli;
    }
}

class ip_address {

    function get() {

        if ( $_ENV["HTTP_CLIENT_IP"] && strcasecmp( $_ENV["HTTP_CLIENT_IP"], "unknown" ) ) {
            $ip = $_ENV["HTTP_CLIENT_IP"];
        } else {
            if ( $_ENV["HTTP_X_FORWARDED_FOR"] && strcasecmp( $_ENV["HTTP_X_FORWARDED_FOR"], "unknown" ) ) {
                $ip = $_ENV["HTTP_X_FORWARDED_FOR"];
            } else {
                if ( $_ENV["REMOTE_ADDR"] && strcasecmp( $_ENV["REMOTE_ADDR"], "unknown" ) ) {
                    $ip = $_ENV["REMOTE_ADDR"];
                } else {
                    if ( isset( $_SERVER["REMOTE_ADDR"] ) && $_SERVER["REMOTE_ADDR"] && strcasecmp( $_SERVER["REMOTE_ADDR"], "unknown" ) ) {
                        $ip = $_SERVER["REMOTE_ADDR"];
                    } else {
                        $ip = "unknown";
                    }
                }
            }
        }

        return $ip;
    }
}
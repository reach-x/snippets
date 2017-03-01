<?php

$databases = get_database_configs();

print_r(preg_grep_keys('/_dev/', $databases));
print_r(preg_grep_keys('/_dev/', $databases, PREG_GREP_INVERT));

function preg_grep_keys($pattern, $input, $flags = 0) {

	return array_intersect_key($input, array_flip(preg_grep($pattern, array_keys($input), $flags)));
}

function get_database_configs($base_connection = array()) {

	return array(
		'lbc' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => '<database_name>',
		)),
		'lbc_dev' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'lbc_dev',
		)),
		'admin' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'admin',
		)),
		'admin_dev' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'admin_dev',
		)),
		'clients' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'clients',
		)),
		'clients_dev' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'clients_dev',
		)),
		'cbs' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'cbs',
		)),
		'cbs_dev' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'cbs_dev',
		)),
		'mcmc' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'mcmc',
		)),
		'mcmc_dev' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'mcmc',
		)),
		'olap' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'olap',
		)),
		'olap_dev' => array_merge($base_connection, array(
			'hostname' => 'db02.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'olap_dev',
		)),
		'openmail' => array_merge($base_connection, array(
			'hostname' => 'db03.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'openmail',
		)),
		'openmail_dev' => array_merge($base_connection, array(
			'hostname' => 'db03.mypm.us',
			'username' => 'root',
			'password' => '<password>',
			'database' => 'openmail_dev',
		)),

	);
}

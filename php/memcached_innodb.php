<?php

// Identical config/code for memcached vs InnoDB
$frontendOpts = array(
  'caching' => true,
  'lifetime' => 3600,
  'automatic_serialization' => true
);
$memcacheOpts = array(
  'servers' =>array(
    array(
      'host'   => 'db02.mypm.us',
      'port'   => 11211,
      'weight' => 1,
    )
  ),
  'client' => array(
    'compression' => true,
  ),
);
$cache = Zend_Cache::factory('Core', 'Libmemcached', $frontendOpts, $memcacheOpts);
$timer->start();
for ($i = 0; $i < 100000; $i++) {
  $cache->save(new Object(), "key_$i");
}
$totalTimeStore = $timer->stop();
$avgTimeStore = $totalTimeStore / 100000;
$timer->start();
for ($i = 0; $i < 10; $i++) {
  for ($i = 0; $i < 100000; $i++) {
    $obj = $cache->load("key_$i");
  }
}
$totalTimeFetch = $timer->stop();
$avgTimeFetch = $totalTimeFetch / 1000000;

printf("totalTimeFetch: %s avgTimeFetch: %s \n",$totalTimeFetch, $avgTimeFetch);


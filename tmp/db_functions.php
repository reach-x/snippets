<?php
function db_connect($database)
{
   $result = @mysql_connect('localhost', 'imcscd_savedb', '1*x!scddb!01'); 
   if (!$result)
      return false;
   if (!@mysql_select_db('imcscd_'.$database))
      return false;
   return $result;
}

function db_result_to_array($result)
{
   $res_array = array();

   for ($count=0; $row = @mysql_fetch_array($result); $count++)
     $res_array[$count] = $row;

   return $res_array;
}
function db_select($fields, $table, $filter = '', $database)
{
   $conn = db_connect($database);
   $query = 'select '.$fields.' from '.$table.' '.$filter; 
   $result = @mysql_query($query);
   if (!$result)
     return false;
   $num_rows = @mysql_num_rows($result);
   if ($num_rows ==0)
      return false;  
   $result = db_result_to_array($result);
   return $result; 
}
function db_insert($fields, $table, $values, $database)
{
   $conn = db_connect($database);
   $query = 'insert into '.$table.' ('.$fields.') values ('.$values.')'; 
   $result = @mysql_query($query);
   if (!$result)
     return false;
   //$result = db_result_to_array($result);
   $result = mysql_insert_id();
   return $result; 
}
function db_update($fields, $table, $filter)
{
   $conn = db_connect($database);
   $query = 'update '.$table.' SET '.$fields.' where '.$filter; 
   $result = @mysql_query($query);
   if (!$result)
     return false;
   //$result = db_result_to_array($result);
   $result = mysql_insert_id();
   return $result; 
}
function db_delete($table, $filter)
{
   $conn = db_connect($database);
   $query = 'delete from '.$table.' where '.$filter; 
   $result = @mysql_query($query);
   if (!$result)
     return false;
   return $result; 
}
?>
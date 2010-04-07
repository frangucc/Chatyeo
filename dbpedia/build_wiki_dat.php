<?
ini_set('memory_limit', '64000M');

$db = mysql_connect("localhost","root","100%talk");
mysql_select_db("chatyeo");

$sql="select L.value,wiki_dat.links from wiki_dat left join lookup L on L.term=wiki_dat.term  ";
$fetch=mysql_query($sql,$db) or die(mysql_error());

function fix_erlang($data){
   //  $data = str_replace('"','\"',$data);
     $data = str_replace("\\","\\\\",$data);
  return ($data);
}

while($row=mysql_fetch_row($fetch)){
 $term = $row[0]; $links=$row[1];
 $term = fix_erlang($term);
 $links = fix_erlang($links);

 echo "{\"$term\",[$links]}.\r\n"; 

}



?>

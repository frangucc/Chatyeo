<?
ini_set('memory_limit', '64000M');

$f = file("articles_label_en.nt");

$i=0;


$db = mysql_connect("localhost","root","100%talk");
mysql_select_db("chatyeo");


foreach ($f as $line){ 
  $line=strtolower($line);
  $broken = split("> <",$line);
  if(sizeof($broken)==2){
    $b2 = split("> \"",$broken[1]); 
    $linkname = substr($broken[0],1);
    $b3 = split("\"",$b2[1]);
    $label = $b3[0]; 
  //   $lookup[$linkname] = $label; 
    $linkname = addslashes($linkname);
    $label = addslashes($label);
    $fetch = mysql_query("REPLACE DELAYED INTO lookup (term,value) values ('$linkname','$label')",$db) or die(mysql_error());


  } else {

  }  
  $i++;
}

unset($f);


?>

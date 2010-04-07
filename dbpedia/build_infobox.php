<?
ini_set('memory_limit', '6400M');


$db = mysql_connect("localhost","root","100%talk");
mysql_select_db("chatyeo");

function FetchValue($SQL){
   global $db;
   $fetch = mysql_query($SQL,$db) or die(mysql_error());
   if($row=mysql_fetch_row($fetch)){
      $result=$row[0];
   } else {
   $result="";
   }
   return $result;
}

function FetchLookup($term){
  $term = addslashes($term);
  return FetchValue("select value from lookup where term='$term'");
}


echo "Loading full file...\r\n";



$f = file("infobox-mappingbased-loose.nt");

$i=0;

foreach ($f as $line){ 
  $line=strtolower($line);
  $broken = split("> <",$line);
  $linkname = substr($broken[0],1);
  if(sizeof($broken)==2){
    $b2 = split("> \"",$broken[1]);     
    $b3 = split("\"",$b2[1]);
    $label = $b3[0];  
    if(!is_numeric($label)){
         $term = addslashes( $linkname );
         $link = "\"$label\"";
         $links = FetchValue("select links from wiki_dat where term='$term'");
         if (trim($links)==""){
             $links=$link;
         } else $links = $link . "," . $links;
         $links = addslashes($links);
          //  echo $lookup[$linkname] . "|$label|" . $label . "\r\n";
         $sql = "replace delayed into wiki_dat (term,links) values ('$term','$links')";   
         mysql_unbuffered_query($sql) or die(mysql_error());        

    }
  } else {
        $label_a = split(">",$broken[2]);
        $label = $label_a[0];
      //  echo $lookup[$linkname] . "|$label|" . $lookup[$label] . "\r\n";

         $term = addslashes( $linkname );
         $link = "\"".FetchLookup($label)."\"";
         $links = FetchValue("select links from wiki_dat where term='$term'");
         if (trim($links)==""){
             $links=$link;
         } else $links = $link . "," . $links;
         $links = addslashes($links);
          //  echo $lookup[$linkname] . "|$label|" . $label . "\r\n";
         $sql = "replace delayed into wiki_dat (term,links) values ('$term','$links')";   
         mysql_unbuffered_query($sql) or die(mysql_error());       


  }  
  $i++;
}

unset($f);




?>

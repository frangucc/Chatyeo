<?
ini_set('memory_limit', '64000M');
 
$file = "wiki_base.dat";
$fh = fopen($file, "rb");

$i=0;
$pieces = array();

while(!feof($fh)) {
  $line = strtolower(fgets($fh)); 
  $broken = split("\|",$line);
  $item = $broken[0];
  $category = trim($broken[2]);
  if($lastitem!=$item){
    $items = implode(",",$pieces);
    if($lastitem!=""){
       $lastitem = str_replace('"','\"',$lastitem);
       $lastitem = str_replace("\\","\\\\",$lastitem);
       echo strtolower( "{\"" . $lastitem . "\",[" . $items . "]}.\r\n");     
    } 
    unset($pieces);
    $pieces = array();
  } 
    $lastitem = $item;


     $category = str_replace('"','\"',$category);
     $category = str_replace("\\","\\\\",$category);

    if($category!=""){ $pieces[]="\"$category\"";}
  $i++;
}

unset($f);


?>

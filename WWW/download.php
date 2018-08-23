<?php
$filename = $_GET['file'];
$build_hash = $_GET['build'];

// Configuration file should be symbolic link to the config of installed festral
$festral_conf_path = '.festral.conf';

$config_contents = file_get_contents($festral_conf_path);
$config = json_decode($config_contents, true);

$file_path = $config['buildLogDir'] . '/' . $build_hash;
$download_file = $file_path . '/' . $filename;

if(!empty($filename)){
    // Check file is exists on given path.
    if(file_exists($download_file))
    {
      header('Content-Disposition: attachment; filename=' . $filename);  
      readfile($download_file); 
      exit;
    }
    else
    {
      echo 'File does not exists on given path';
    }
 } 
?>

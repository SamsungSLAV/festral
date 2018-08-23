<?php
require "funcs.php";

$type = $_GET["type"];
$hash = $_GET["hash"];
$time = $_GET["time"];

$festral_conf_path = '.festral.conf';
$config_contents = file_get_contents($festral_conf_path);
$conf = json_decode($config_contents, true);

if (!($type == "build" || $type == "test")) exit(1);
if (!(strlen($hash) == 40)) exit(2);
if (!(strlen($time) == 14)) exit(3);

$log_dir_prefix = '';

if ($type == "build") $log_dir_prefix = $conf['buildLogDir'];
if ($type == "test") $log_dir_prefix = $conf['testLogDir'];

$log_dir = $log_dir_prefix."/".$hash."_".$time;

if ($type == "build")
	$f=$log_dir."/build.log";
else
	$f=$log_dir."/tf.log";

if (!is_dir($log_dir) || !file_exists($f)) {
    header("HTTP/1.0 404 Not Found");
	echo "File $f not fund";
    exit;
}
//header("Content-Type: text/plain; charset=utf-8");
header("Content-Type: text/html; charset=utf-8");
header("Content-Disposition: inline; filename=\"$type.txt\"");
header("Content-Transfer-Encoding: binary");
echo "<pre>";
readfile($f);
echo "</pre>";
?>


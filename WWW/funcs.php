<?php
define("DIR", dirname(__FILE__));

function console_log($text) {
    echo "<script>console.log(".json_encode($text).")</script>";
}
function server_log($msg) {
	fwrite(STDERR,$msg);
}

function startsWith($str, $part) {
     $length = strlen($part);
     return (substr($str, 0, $length) === $part);
}
function endsWith($str, $part) {
    $length = strlen($part);
    if ($length == 0) return true;
    return (substr($str, -$length) === $part);
}

function move_or_copy($src,$dst) {
	$r=move_uploaded_file($src, $dst);
	if (!$r) {
		$r=copy($src, $dst);
	}
	else  {
		echo "move_uploaded_file successful\n";
	}
	return $r;
}

function run_cmd($cmd,$cwd=".") {
	//echo "exec: $cmd";
	$descriptorspec = array(
	   0 => array("pipe", "r"),  // stdin is a pipe that the child will read from
	   1 => array("pipe", "w"),  // stdout is a pipe that the child will write to
	   //2 => array("pipe", "w"), // stderr
	   //2 => array("file", "/tmp/error-output.txt", "a") // stderr is a file to write to
	);
	$env = array();
	$r=null;
	$proc = proc_open($cmd, $descriptorspec, $pipes, $cwd, $env);
	if (is_resource($proc)) {
		fclose($pipes[0]);
		$r = stream_get_contents($pipes[1]);
		fclose($pipes[1]);
		$return_value = proc_close($proc);
	}
	return $r;
}
?>

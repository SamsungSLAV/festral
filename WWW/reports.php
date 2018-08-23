<?php
$reports_dir="reports";
$reports = array();

if ($handle = opendir($reports_dir)) {
    while (false != ($entry = readdir($handle))) {
        array_push($reports, $entry);
    }
    closedir($handle);
}

sort($reports);

foreach ($reports as $report) {
    if ($report != "." && $report != "..") {
        echo "<a href=\"$reports_dir/$report\">$report</a><br/>";
    }
}
?>


var fullTables = {}

function arrayify(collection) {
  return Array.prototype.slice.call(collection);
}

function factory(headings) {
  return function(row) {
    return arrayify(row.cells).reduce(function(prev, curr, i) {
      prev[headings[i]] = curr.outerHTML;
      return prev;
    }, {});
  }
}

function parseTable(table) {
  var headings = arrayify(table.tHead.rows[0].cells).map(function(heading) {
    return heading.innerText;
  });
  return arrayify(table.tBodies[0].rows).map(factory(headings));
}

function colors(x) {
    switch(x) {
        case "BUILD FAILED":
        case "DOWNLOAD FILES ERROR" :
        case "NO RESULTS" :
        case "YAML NOT FOUND" :
        case "NO JOB STARTED" :
        case "DEVICE FAILED" :
        case "WELES ERROR" :
            return 'red';
    }
    if (x.includes("SEGFAULT")) return 'red';
    else {
        var y = x.split("/");
        var pass = parseInt(y[0]);
        var all = parseInt(y[1]);
        return ("rgb(" + (150 - ((150/all) * pass) )
                + "," + ((150/all) * pass)  + ",0)");
    }
}

function colorize(testTable) {
    var tab = document.getElementById(testTable);
    var rows = Array.from([tab.tBodies[0].children][0]);
    rows.map(row => row.children[3].style.color = colors(row.children[3].textContent));
}

function init(buildTable, testTable) {
    var bt = document.getElementById(buildTable);
    var tt = document.getElementById(testTable);
    fullTables[buildTable] = parseTable(bt);
    fullTables[testTable] = parseTable(tt);
    colorize(testTable);
}

function filterTable(tabName, field, value) {
    regex = new RegExp(value);
    return fullTables[tabName].filter(function(row) {
        return row[field].match(value);
    });
}

function getTable(objList) {
    extractObj = row => (acc, key, i) => acc + row[key];
    return objList.reduce(function(acc, row, i) {
        return acc + "<tr>" + Object.keys(row).reduce(extractObj(row), "")
            + "</tr>";
    }, "");
}

function showTable(oldTab, newTBody) {
    document.getElementById(oldTab).tBodies[0].innerHTML = newTBody;
    colorize(oldTab);
}

function filterAllTables(field, value) {
    return zipWith (showTable) (Object.keys(fullTables))
        (Object.keys(fullTables).map(x =>
                                     getTable(filterTable(x, field, value))));
}

var zipWith = (f) => (xs) => (ys) => xs.map((n,i) => f(n, ys[i]))

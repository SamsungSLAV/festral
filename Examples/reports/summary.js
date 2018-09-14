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

function init(buildTable, testTable) {
    var bt = document.getElementById(buildTable);
    var tt = document.getElementById(testTable);
    fullTables[buildTable] = parseTable(bt);
    fullTables[testTable] = parseTable(tt);
}

function filterTable(tabName, field, value) {
    return fullTables[tabName].filter(function(row) {
        return row[field].includes(value);
    });
}

function getTable(objList) {
    extractObj = row => (acc, key, i) => acc + row[key];
    return objList.reduce(function(acc, row, i) {
        return acc + "<tr>" + Object.keys(row).reduce(extractObj(row), "") + "</tr>";
    }, "");
}

function showTable(oldTab, newTBody) {
    document.getElementById(oldTab).tBodies[0].innerHTML = newTBody;
}

function filterAllTables(field, value) {
    return zipWith (showTable) (Object.keys(fullTables)) (Object.keys(fullTables).map(x => getTable(filterTable(x, field, value))));
}

var zipWith = (f) => (xs) => (ys) => xs.map((n,i) => f(n, ys[i]))

function arrayify(collection) {
  return Array.prototype.slice.call(collection);
}

function factory(headings) {
  return function(row) {
    return arrayify(row.cells).reduce(function(prev, curr, i) {
      prev[headings[i]] = curr.innerText;
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

function printTable(table) {
    var tab = document.getElementById(table);
    console.log(parseTable(tab));
}

function filterTable(table, field, value) {
    var tab = document.getElementById(table);
    tab.reduce(function (pre, curr, i, array) {
        if (!curr[field].includes(value)) {
            tab.deleteRow(i - pre);
            return ++pre;
        }
    }, 0);
}

let rotate = (array) =>
  array[0].map((val, index) => array.map((row) => row[index]).reverse());

function generateTableHead(table, data) {
    let thead = table.createTHead();
    let row = thead.insertRow();
    for (let key of data) {
      let th = document.createElement("th");
      let text = document.createTextNode(key);
      th.appendChild(text);
      row.appendChild(th);
    }
}

function generateTable(data, highlighted_rows) {
    let table = document.createElement("table");
    table.classList.add("table");
    for (let element of data) {
        let row = table.insertRow();
        if (highlighted_rows.includes(element[0])) {
            row.classList.add("table-active");
        }
        for (let key of element) {
            let cell = row.insertCell();
            let text = document.createTextNode(key);
            cell.appendChild(text);
        }
    }
    document.querySelector("main").appendChild(table);
}
for (let plot of data) {
    if(plot.type=="table"){
        generateTable(plot.cells, plot.highlighted_rows);
    } else {
        throw new Error('Unknown plot type');
    }
}

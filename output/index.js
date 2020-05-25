let rotate = (array) =>
  array[0].map((val, index) => array.map((row) => row[index]));

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

// http://blog.adamcole.ca/2011/11/simple-javascript-rainbow-color.html
function rainbowStop(h) 
{
    let f= (n,k=(n+h*12)%12) => .5-.5*Math.max(Math.min(k-3,9-k,1),-1);  
    let rgb2hex = (r,g,b) => "#"+[r,g,b].map(x=>Math.round(x*255).toString(16).padStart(2,0)).join('');
    return ( rgb2hex(f(0), f(8), f(4)) );
}

function generatePlot(data){
    let canvas = document.createElement("canvas");
    canvas.style.width="800px";
    canvas.style.height="600px";

    let max_length=data[0].length;
    let labels=new Array(max_length);
    for(let i=0; i<max_length; i++){
        labels[i]=i;
    }

    let converted_data={
        labels:labels,
        
        datasets:data.map((element, index)=>{
            let color=rainbowStop(0.15*index)+"50";
            return {
                borderColor: color,
                backgroundColor: color,
                data:element,
                label:index.toString(),
                fill: false,
            }
        })
    }

    Chart.Line(canvas.getContext('2d'), {
        data: converted_data,
    })
    document.querySelector("main").appendChild(canvas);
}

for (let plot of data) {
    if(plot.type=="table"){
        generateTable(plot.cells, plot.highlighted_rows);
    } else if(plot.type=="plot"){
        generatePlot(plot.data);
    } else {
        throw new Error('Unknown plot type');
    }
}

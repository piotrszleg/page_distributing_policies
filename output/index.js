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
function makeCanvas(){
    let canvas = document.createElement("canvas");
    canvas.style.width="800px";
    canvas.style.height="600px";
    document.querySelector("main").appendChild(canvas);
    return canvas;
}
function linePlot(data, xAxis, yAxis){
    let canvas = makeCanvas();

    let max_length=data[0].length;
    let labels=new Array(max_length);
    for(let i=0; i<max_length; i++){
        labels[i]=i;
    }
    let colorsCount=data.length;
    let colorStep=360/colorsCount;// hue is in range 0-360
    let saturation=40;
    let lightness=100;
    let converted_data={
        labels:labels,
        datasets:data.map((element, index)=>{
            let color=colorsys.hsv2Hex(index*colorStep, saturation, lightness);
            return {
                borderColor: color,
                backgroundColor: color,
                data:element,
                label:index.toString(),
                fill: false,
            }
        })
    }

    let scales={}
    if (xAxis!=undefined){
        scales.xAxes=[{
            scaleLabel: {
              display: true,
              labelString: xAxis
            }
        }]
    }
    if(yAxis!=undefined){
        scales.yAxes=[{
            scaleLabel: {
              display: true,
              labelString: yAxis
            }
        }]
    }

    Chart.Line(canvas.getContext('2d'), {
        data: converted_data,
        options:{scales:scales}
    })
    
}

function scatterPlot(data, xAxis, yAxis){
    let canvas = makeCanvas();

    let scales={}
    if (xAxis!=undefined){
        scales.xAxes=[{
            scaleLabel: {
              display: true,
              labelString: xAxis
            }
        }]
    }
    if(yAxis!=undefined){
        scales.yAxes=[{
            scaleLabel: {
              display: true,
              labelString: yAxis
            }
        }]
    }

    Chart.Scatter(canvas.getContext('2d'), {
        data: {datasets: data.map(element=>({data:element}) )},
        options:{scales:scales}
    })
}

for (let plot of data) {
    if(plot.type=="table"){
        generateTable(plot.cells, plot.highlighted_rows);
    } else if(plot.type=="line"){
        linePlot(plot.data, plot.xAxis, plot.yAxis);
    } else if(plot.type=="scatter"){
        scatterPlot(plot.data, plot.xAxis, plot.yAxis);
    } else {
        throw new Error('Unknown plot type');
    }
}

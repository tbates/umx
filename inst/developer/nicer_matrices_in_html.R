caption = 'Table 1: Matrix A'
datatable(umx_round(m1$A$values, 2), rownames = T, caption = caption)

options = options = list(columnDefs = list(list(className = 'dt-center', targets = 4))
  
umx_show_matrix(colnames = NULL, rownames = NULL)


datatable(
  iris, extensions = 'Buttons', options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
  )
)


var dataset = [],
tmpDataset  = [],
i, j;

for (i = 0; i < 5; i++) {
    for (j = 0, tmpDataset = []; j < 3; j++) {
        tmpDataset.push("Row:"+i+",Col:"+j);
    }
    dataset.push(tmpDataset);
}
	
d3.select("#viz")
    .append("table")
    .style("border-collapse", "collapse")
    .style("border", "2px black solid")
    
    .selectAll("tr")
    .data(dataset)
    .enter().append("tr")
    
    .selectAll("td")
    .data(function(d){return d;})
    .enter().append("td")
    .style("border", "1px black solid")
    .style("padding", "10px")
    .on("mouseover", function(){d3.select(this).style("background-color", "aliceblue")}) 
    .on("mouseout", function(){d3.select(this).style("background-color", "white")}) 
    .text(function(d){return d;})
    .style("font-size", "12px");
	
	
	
	> umx_check_parallel(1:8, row = TRUE, numberSubjects = 100)
	1/8
	You have been using 1 of 8 available cores (0 means max - 1)
	I will now set cores to '1', '2', '3', '4', '5', '6', '7', and '8' (they will be reset after) and run a script that hits that many cores if possible.
	Check CPU while it's running and see if R is pegging the processor.
	Running nCcores_equals_1 with 32 parameters
	Running nCcores_equals_2 with 32 parameters
	Running nCcores_equals_3 with 32 parameters
	Running nCcores_equals_4 with 32 parameters
	Running nCcores_equals_5 with 32 parameters
	Running nCcores_equals_6 with 32 parameters
	Running nCcores_equals_7 with 32 parameters
	Running nCcores_equals_8 with 32 parameters
	nCcores_equals_1: 09.07 seconds
	nCcores_equals_2: 04.96 seconds(∆: -4.103)
	nCcores_equals_3: 03.57 seconds(∆: -5.493)
	nCcores_equals_4: 02.90 seconds(∆: -6.168)
	nCcores_equals_5: 02.81 seconds(∆: -6.254)
	nCcores_equals_6: 02.83 seconds(∆: -6.234)
	nCcores_equals_7: 02.81 seconds(∆: -6.254)
	nCcores_equals_8: 02.81 seconds(∆: -6.256)
	
	nCcores_equals_1: 01 minute 43.13 seconds
	nCcores_equals_4: 00 minute 31.76 seconds (∆: -71.37)

	nCcores_equals_4: 32.58 seconds (row =)
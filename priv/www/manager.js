// jQuery things
$(document).ready(function(){
	createTable();
	$("#refreshform").submit(function(){
		createTable();
	});
});

function createTable()
{
	$.ajax({
		url:"manager",
		type:"GET",
		timeout:6000,
		success: function(data) {
			$("#processestable").dataTable({
				aaData:data.aaData,
				bProcessing:true,
				bDestroy:true
			});
		},
		error: function(data, error) {
			alert("Error: " + error);
		}
	});
}

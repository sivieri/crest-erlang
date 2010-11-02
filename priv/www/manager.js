// jQuery things
$(document).ready(function()
{
	$("#refreshform").submit(function(){
		$.ajax({
			url:$("#form2div form").attr("action"),
			type:"GET",
			timeout:6000,
			success: function(data) {
				
			},
			error: function(data, error) {
    			alert("Error: " + error);
    		}
		});
	});
});

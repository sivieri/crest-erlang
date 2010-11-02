// jQuery things
$(document).ready(function(){
	$("#processestable").dataTable({
		sAjaxSource:"manager",
		fnServerData:function(sSource, aoData, fnCallback){
			$.ajax( {
				type:"GET",
				url:sSource,
				timeout:6000,
				success:fnCallback
			});
		}
	} );
});

// jQuery things
$(document).ready(function()
{
	$("input[name='demo']").change(function(){
	    if ($("input[name='demo']:checked").val() == 'word')
	    {
	    	$("#form2div").show('slow');
	    	$("#limit").show('fast');
	    	$("#limitlabel").show('fast');
	    }
	    else if ($("input[name='demo']:checked").val() == 'idf')
	    {
	    	$("#form2div").show('slow');
	    	$("#limit").hide('fast');
	    	$("#limitlabel").hide('fast');
	    }
	    else
	    {
	    	$("#form2div").show('slow');
	    	$("#limit").hide('fast');
	    	$("#limitlabel").hide('fast');
	    }
	    $.ajax({
    		url:"localhost",
    		type:"GET",
    		data:"type=" + $("input[name='demo']:checked").val(),
    		dataType:"text",
    		timeout:6000,
    		success: function(data) {
    			$("#form2div form").attr("action", data);
    			$("#form2div form input[name='Submit']").attr("disabled", false);
    		}
    	});
	});
	$("#form2").submit(function(){
		$.ajax({
			url:$("#form2div form").attr("action"),
			type:"POST",
			data:"",
			dataType:"json",
			timeout:6000,
			success: function(data) {
				
			}
		});
	});
});

// copyright 1999 Idocs, Inc. http://www.idocs.com
// Distribute this script freely but keep this notice in place
function numbersonly(myfield, e, dec)
{
	var key;
	var keychar;
	
	if (window.event)
	    key = window.event.keyCode;
	else if (e)
	    key = e.which;
	else
	    return true;
	keychar = String.fromCharCode(key);
	
	// control keys
	if ((key==null) || (key==0) || (key==8) || 
	    (key==9) || (key==13) || (key==27) )
	    return true;
	
	// numbers
	else if ((("0123456789").indexOf(keychar) > -1))
	    return true;
	
	// decimal point jump
	else if (dec && (keychar == "."))
    {
		myfield.form.elements[dec].focus();
		return false;
    }
	else
	    return false;
}

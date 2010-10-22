// jQuery things
$(document).ready(function()
{
	$("input[name='demo']").change(function(){
		$("#form2div form input[name='Submit']").attr("disabled", "true");
	    if ($("input[name='demo']:checked").val() == "word")
	    {
	    	$("#form2div").show("slow");
	    	$("#limitfield").show("fast");
	    }
	    else if ($("input[name='demo']:checked").val() == "idf")
	    {
	    	$("#form2div").show("slow");
	    	$("#limitfield").hide("fast");
	    }
	    else
	    {
	    	$("#form2div").show("slow");
	    	$("#limitfield").hide("fast");
	    }
	    $.ajax({
    		url:"demo",
    		type:"GET",
    		data:"type=" + $("input[name='demo']:checked").val(),
    		dataType:"text",
    		timeout:6000,
    		success: function(data) {
    			$("#form2div form").attr("action", "crest/" + data);
    			$("#form2div form input[name='Submit']").removeAttr("disabled");
    		},
    		error: function(data, error) {
    			alert("Error: " + error);
    		}
    	});
	});
	$("#form2").submit(function(){
		$.ajax({
			url:$("#form2div form").attr("action"),
			type:"POST",
			data:$("#form2").serialize(),
			dataType:"json",
			timeout:6000,
			success: function(data) {
				plotResults(data);
			},
			error: function(data, error) {
    			alert("Error: " + error);
    		}
		});
	});
});

function plotResults(obj)
{
	for(j = 0; j < obj.length; ++j)
	{
		$("#results").append('<div id="result' + j + '" class="result"></div>');
		var values = new Array();
		var terms = new Array();
		for(i = 0; i < obj[j].words.length; ++i)
		{
			values[i] = new Array(obj[j].words[i].frequency, i+1);
			terms.push(obj[j].words[i].word);
		}
		$("#result" + j).height(terms.length*40);
		plot1 = $.jqplot('result' + j, [values], {
		    legend:{show:false, location:'ne'},
		    title:obj[j].ip,
		    seriesDefaults:{
		        renderer:$.jqplot.BarRenderer, 
		        rendererOptions:{barDirection:'horizontal', barMargin:8}, 
		        shadowAngle:135},
		    series:[
		        {label:'Words'}, 
		    ],
		    axes:{
		        xaxis:{min:0}, 
		        yaxis:{
		            renderer:$.jqplot.CategoryAxisRenderer,
		            ticks:terms
		        }
		    }
		});
	}
}

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
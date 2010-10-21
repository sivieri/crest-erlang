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

// Example: [{words:[{word:"and", frequency:10}, {word:"to", frequency:11}, {word:"the", frequency:19}]}]
function plotResults(obj)
{
	values = [];
	terms = [];
	for(i = 0; i < obj[0].words.length; i++)
	{
		values.push(obj[0].words[i].frequency);
		terms.push(obj[0].words[i].word);
	}
	plot1 = $.jqplot('results', [values], {
	    legend:{show:false, location:'ne'},
	    title:'Word frequency',
	    seriesDefaults:{
	        renderer:$.jqplot.BarRenderer, 
	        rendererOptions:{barDirection:'horizontal', barPadding: 6, barMargin:15}, 
	        shadowAngle:135},
	    series:[
	        {label:'Words'}, 
	    ],
	    axes:{
	        xaxis:{min:0}, 
	        yaxis:{
	            renderer:$.jqplot.CategoryAxisRenderer,
	            //ticks:terms
	        }
	    }
	});
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

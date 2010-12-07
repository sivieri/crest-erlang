/* Copyright (c) 2010 Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
 * 
 * This file is part of CREST-Erlang.
 * 
 * CREST-Erlang is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * CREST-Erlang is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with CREST-Erlang. If not, see <http://www.gnu.org/licenses/>.
 */ 

// jQuery things
$(document).ready(function()
{
	$("input[name='demo']").change(function(){
		if($("input[name='demo']:checked").val() == "wordstatus") {
			$("#form2div form input[name='Submit']").attr("value", "Add new addresses");
		}
		else {
			$("#form2div form input[name='Submit']").attr("value", "Launch");
		}
		$("#submit").attr("disabled", "true");
        $("#form2div").show("slow");
	    $.ajax({
    		url:"crest/local/" + $("input[name='demo']:checked").val(),
    		type:"GET",
    		timeout:10000,
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
		$("#results").append('<p><img src="images/ajax-loader.gif" alt="Loading..." class="centered" /></p>');
		$.ajax({
			url:$("#form2div form").attr("action"),
			type:"POST",
			data:$("#form2").serialize(),
			dataType:"json",
			timeout:10000,
			success: function(data) {
				$("#results").children().remove();
				plotResults(data, $("input[name='demo']:checked").val());
			},
			error: function(data, error) {
    			alert("Error: " + error);
    		}
		});
	});
});

function plotResults(obj, type)
{
	if (type == "cosine") {
		var values = new Array();
		var terms = new Array();
		for(i = 0; i < obj.length; ++i)
		{
			values.push(obj[i].value);
			terms.push(obj[i].ip1 + "<br/>" + obj[i].ip2);
		}
		plot = $.jqplot('results', [values], {
		    legend:{show:false, location:'ne', xoffset:55},
		    title:'Cosine similarities',
		    seriesDefaults:{
		        renderer:$.jqplot.BarRenderer, 
		        rendererOptions:{ barMargin: 20},
				shadowAngle:135
		    },
		    series:[
		        {label:'Addresses'}, 
		    ],
		    axes:{
		        xaxis:{
		            renderer:$.jqplot.CategoryAxisRenderer, 
		            ticks:terms
		        }, 
		        yaxis:{min:0, max:1}
		    }
		});
	}
	else {
		for(j = 0; j < obj.length; ++j)
		{
			$("#results").append('<div id="tab' + j + '"><div id="chart' + j + '"></div></div>');
			var values = new Array();
			var terms = new Array();
			var maxVal = 0;
			for(i = 0; i < obj[j].words.length; ++i)
			{
				values[i] = new Array(obj[j].words[i].frequency, i+1);
				if (obj[j].words[i].frequency > maxVal)
					maxVal = obj[j].words[i].frequency;
				terms.push(obj[j].words[i].word);
			}
			$("#result" + j).height(terms.length*40);
			plot = $.jqplot('chart' + j, [values], {
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
			        xaxis:{min:0, max:maxVal}, 
			        yaxis:{
			            renderer:$.jqplot.CategoryAxisRenderer,
			            ticks:terms
			        }
			    }
			});
		}
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

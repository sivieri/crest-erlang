/* Copyright (c) 2010 Alessandro Sivieri <alessandro.sivieri@mail.polimi.it>
 * 
 * This file is part of CREST-Erlang.
 * 
 * CREST-Erlang is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * CREST-Erlang is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with CREST-Erlang. If not, see <http://www.gnu.org/licenses/>.
 */ 

// jQuery things
$(document).ready(function(){
	createTableInstalled();
	createTableLocal();
	$("#refreshform").submit(function(){
		createTableInstalled();
		createTableLocal();
	});
	$("#localtable a").bind("click", function(event){
		event.preventDefault();
		event.stopPropagation();
		$.get(this.href, {}, function(response){
			$("#newlocal").html(response);
		});
		return false;
	});
});

function createTableInstalled()
{
	$.ajax({
		url:"crest/manager/installed",
		type:"GET",
		timeout:10000,
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

function createTableLocal()
{
	$.ajax({
		url:"crest/manager/local",
		type:"GET",
		timeout:10000,
		success: function(data) {
			$("#localtable").dataTable({
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

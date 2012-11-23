function plotMarker(probability, result){
	x_location = (probability*100);
	y_location = (probability*50);
	$("#riskMatrix").drawRect({
	  strokeStyle: "#000",
	  strokeWidth: 5,
	  height: 50,
	  width: 100,
	  x: x_location, y: y_location,
	  fromCenter: false
	});
}

function getColour(dangerint){
	if ((dangerint) > 72){
		return "#FF0000"
	}if ((dangerint) > 55){
		return "#FF3300"
	}if ((dangerint) > 40){
		return "#FF6600"
	}if ((dangerint) > 35){
		return "#FF9900"
	}if ((dangerint) > 20){
		return "#FFCC00"
	}if ((dangerint) > 9){
		return "#FFFF00"
	}if ((dangerint) > 8){
		return "#CCFF00"
	}if ((dangerint) > 6){
		return "#99FF00"
	}if ((dangerint) > 3){
		return "#66FF00"
	}if ((dangerint) > 0){
		return "#33FF00"
	}
}

$(document).ready(function(){

	$( "#tabs" ).tabs({heightStyle: "content"}).addClass( "ui-tabs-vertical ui-helper-clearfix" );
    $( "#tabs li" ).removeClass( "ui-corner-top" ).addClass( "ui-corner-left" );
	$(".ui-tabs .ui-tabs-panel").css("padding", 0);
	
	i = 0;
	globalcolour = "green";
	globalx = 0;
	globaly = 0;
	while (i < 100){
		if (i % 10 == 0 && i != 0){
			globaly += 50;
			globalx = 0;
		}
		dangerint = (globalx/100+1)*(globaly/50+1);
		globalcolour = getColour(dangerint);

		$("#riskMatrix").drawRect({
		  fillStyle: globalcolour,
		  x: globalx, y: globaly,
		  width: 100,
		  height: 50,
		  fromCenter: false
		});
		
		$("canvas").drawText({
		  fillStyle: "#000",
		  strokeStyle: "#000",
		  strokeWidth: 1,
		  x: globalx + 50, y: globaly + 25,
		  font: "16pt Arial",
		  text: dangerint,
		  fromCenter: true,
		  rotate: 0
		});
		
		i += 1;
		globalx += 100;
	}
});

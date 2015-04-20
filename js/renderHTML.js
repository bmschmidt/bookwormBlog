var args = require('system').args,
    page = require('webpage').create(),
    fs = require('fs')
    hash = args[1],
    url = args[2];


var destination="images/" + hash + ".png"

if (!fs.exists(destination)) {
    page.onConsoleMessage = function (msg) {
	//console.log(msg);
    };

    page.viewportSize = {
	width: 900,
	height: 650
    };


    page.onLoadFinished = function() {
	setTimeout(function() {
	    page.render(destination)
	    page.evaluate(function() {
		console.log(document.documentElement.outerHTML);
	    });
	    phantom.exit();
	},10000)
    }
    
    page.open("http://localhost/BookwormD3/plot#" + url);
} else {
    phantom.exit()
}

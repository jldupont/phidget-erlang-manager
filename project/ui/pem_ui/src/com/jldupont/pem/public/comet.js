/**
 * @file comet.js
 * @author Jean-Lou Dupont
 * @depends xmlhttprequest.js
 */
var __cometCmdQueue = [];

(function(){


	function doGetStatus() {
		
		var oXMLHttpRequest	= new XMLHttpRequest;
		oXMLHttpRequest.open("GET", "/status", true);

		/** 
		 * @todo throttle, backoff? 
		 */
		oXMLHttpRequest.onreadystatechange	= function() {
			
			// response from server, queue it & restart
			if (this.readyState == XMLHttpRequest.DONE) {
				// my code
			}
			
 
			 //error... just restart

			
		}
		oXMLHttpRequest.send(null);
		
	}//

	/**
	 * Dequeue command from queue & dispatch 
	 */
	function doDispatch() {
		
	}//
	
	
	// start the ball rolling
	
	
})();


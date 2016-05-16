/* Code derived from  https://github.com/kurtharriger/elm-battleship */

var _Zimmi48$logicgame$Native_DragDrop = function() {

    function property(key, value) {
	return {
            key: key,
	    value: value
	};
    }
    
    function onDragStart(options, decoder, createMessage) {

	function eventHandler(event) {
	    event.dataTransfer.setDragImage(event.target, 0, 0);
	    // This stupid setData call is necessary to make drag and drop work on FF
	    event.dataTransfer.setData("text","");

	    var value = A2(Json.runDecoderValue, decoder, event);
	    if (value.ctor === 'Ok') {
		if (options.stopPropagation) {
		    event.stopPropagation();
		}
		if (options.preventDefault) {
		    event.preventDefault();
		}
		Signal.sendMessage(createMessage(value._0));
	    }
	}
	return property('ondragstart', eventHandler);
    }

    function onDragOver(options, decoder, createMessage) {

	function eventHandler(event)
	{
	    
	    var value = A2(Json.runDecoderValue, decoder, event);
	    if (value.ctor === 'Ok') {
		if (options.stopPropagation) {
		    event.stopPropagation();
		}
		if (options.preventDefault) {
		    event.preventDefault();
		}
                if (typeof(options.dropEffect) == "string") {
                    event.dataTransfer.dropEffect = options.dropEffect;
                }
		Signal.sendMessage(createMessage(value._0));
	    }
	}
	return property('ondragover', eventHandler);
    }

    return {
	onDragStart: F3(onDragStart),
	onDragOver: F3(onDragOver)
    };

}();

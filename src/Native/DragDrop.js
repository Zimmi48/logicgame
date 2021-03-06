/* Code derived from  https://github.com/kurtharriger/elm-battleship */

Elm.Native.DragDrop = {};

Elm.Native.DragDrop.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.DragDrop = localRuntime.Native.DragDrop || {};
	if (localRuntime.Native.DragDrop.values) {
		return localRuntime.Native.DragDrop.values;
	}

  var Json = Elm.Native.Json.make(localRuntime);
  var Signal = Elm.Native.Signal.make(localRuntime);
  var VirtualDom = Elm.Native.VirtualDom.make(localRuntime);

  function property(key, value)
	{
		return {
			key: key,
			value: value
		};
	}

  function onDragStart(options, decoder, createMessage)
	{
    var name = 'dragstart';
		function eventHandler(event)
		{
      event.dataTransfer.setDragImage(event.target, 0, 0);
      // This stupid setData call is necessary to make drag and drop work on FF
      event.dataTransfer.setData("text","");

			var value = A2(Json.runDecoderValue, decoder, event);
			if (value.ctor === 'Ok')
			{
				if (options.stopPropagation)
				{
					event.stopPropagation();
				}
				if (options.preventDefault)
				{
					event.preventDefault();
				}
				Signal.sendMessage(createMessage(value._0));
			}
		}
		return property('on' + name, eventHandler);
	}

  function onDragOver(options, decoder, createMessage)
	{
    var name = 'dragover';
		function eventHandler(event)
		{

			var value = A2(Json.runDecoderValue, decoder, event);
			if (value.ctor === 'Ok')
			{
				if (options.stopPropagation)
				{
					event.stopPropagation();
				}
				if (options.preventDefault)
				{
					event.preventDefault();
				}
                if (typeof(options.dropEffect) == "string")
                {
                    event.dataTransfer.dropEffect = options.dropEffect;
                }
				Signal.sendMessage(createMessage(value._0));
			}
		}
		return property('on' + name, eventHandler);
	}

  return localRuntime.Native.Json.values = {
    onDragStart: F3(onDragStart),
    onDragOver: F3(onDragOver)
  };
};

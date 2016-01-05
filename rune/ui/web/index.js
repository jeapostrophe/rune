window.onload = function() {
    // XXX set from Racket
    var exampleSocket = new WebSocket("ws://localhost:7334/rune");
    window.onkeydown = function (e) {
        exampleSocket.send(
            JSON.stringify(
                ["key!", e.keyIdentifier,
                 e.altKey, e.ctrlKey, e.metaKey,
                 e.shiftKey]));
        e.preventDefault();
    };
    exampleSocket.onopen = function (event) {
        exampleSocket.send(JSON.stringify(["open!"]));
    };
    exampleSocket.onmessage = function (event) {
        eval(event.data);
    };
};

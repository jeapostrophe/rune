// 

var exampleSocket = new WebSocket("ws://localhost:7333/rune");
exampleSocket.onopen = function (event) {
    exampleSocket.send(JSON.stringify({who:"ami"}));
};
exampleSocket.onmessage = function (event) {
    eval(event.data);
}

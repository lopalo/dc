
var CHECK_PERIOD = 5; //milliseconds
var INPUT_DELAY = 400; //milliseconds


function Connection() {
    this._ws = null;
    this._requestCounter = 1;
    this._inputQueue = [];
    this._checkInputId = null;
    this.connected = false;
    _.bindAll(this, "_checkInput");
}
_.extend(Connection.prototype, Backbone.Events);
_.extend(Connection.prototype, {
    _send: function (cmd, body, requestNumber) {
        //requestNumber = 0 means no request
        this._ws.send(JSON.stringify([cmd, body, requestNumber]));
    },
    send: function (cmd, body) {
        this._send(cmd, body, 0);
    },
    request: function (cmd, body, onResponse, context) {
        var num = this._requestCounter;
        var event;
        this._requestCounter += 1;
        this._send(cmd, body, num);
        event = "response:" + num;
        return this.once(event, onResponse, context);
    },
    connect: function (address, timeout, onOpen, onTimeout) {
        var self = this;
        var ws;
        var timeoutId;
        function _onTimeout () {
            self._ws = null;
            onTimeout();
        }
        timeoutId = setTimeout(_onTimeout, timeout);
        this._ws = ws = new WebSocket(address);
        ws.onopen = function () {
            clearTimeout(timeoutId);
            self._checkInputId = setTimeout(self._checkInput, CHECK_PERIOD);
            self.connected = true;
            onOpen();
        }
        ws.onmessage = function (event) {
            self._inputQueue.push(JSON.parse(event.data));
        }
        ws.onclose = function () {
            if (self._checkInputId !== null) {
                clearTimeout(self._checkInputId)
            }
            self._ws = null;
            self._checkInputId = null
            self.connected = false;
        }
    },
    _checkInput: function () {
        var inputQueue = this._inputQueue;
        var period = CHECK_PERIOD;
        var data;
        var cmd;
        var body;
        var parts;
        if (_.random(0, 3) == 0) {
            period += INPUT_DELAY;
        }
        this._checkInputId = setTimeout(this._checkInput, period);
        while (!(_.isEmpty(inputQueue))) {
            data = inputQueue.shift();
            cmd = data[0];
            body = data[1];
            if (_.contains(cmd, '.')) {
                parts = cmd.split('.');
                this.trigger(parts[0], {cmd: parts[1], body: body});
            }
            else {
                this.trigger(cmd, body)
            }
        }
    }
});

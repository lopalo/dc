
define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");

    var settings = require("json!settings.json");
    var checkPeriod = settings.connection["check-period-msec"];
    var freezeFactor = settings.connection["freeze-factor"];
    var inputDelay = settings.connection["input-delay-msec"];


    function Connection() {
        this._ws = null;
        this._requestCounter = 1;
        this._inputQueue = [];
        this._checkInputId = null;
        this._multipartCommands = {};
        this.connected = false;
        this.lastError = "";
        _.bindAll(this, "_checkInput", "close");
    }
    Connection.prototype = Object.create(Backbone.Events);
    _.extend(Connection.prototype, {
        constructor: Connection,
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
        close: function () {
            this._ws.close();
        },
        connect: function (address, timeout, onOpen, onTimeout) {
            var self = this;
            var ws;
            var timeoutId;
            function _onTimeout() {
                self._ws = null;
                onTimeout(self);
            }
            timeoutId = window.setTimeout(_onTimeout, timeout);
            this._ws = ws = new WebSocket(address);
            ws.onopen = function () {
                window.clearTimeout(timeoutId);
                self._checkInputId = window.setTimeout(self._checkInput,
                                                            checkPeriod);
                self.connected = true;
                onOpen(self);
            };
            ws.onmessage = function (event) {
                self._inputQueue.push(JSON.parse(event.data));
            };
            ws.onclose = function () {
                if (self._checkInputId !== null) {
                    window.clearTimeout(self._checkInputId);
                }
                self._ws = null;
                self._checkInputId = null;
                self.connected = false;
                self.trigger("disconnection");
                self.off();
                console.log("Connection closed");
            };
        },
        _checkInput: function () {
            var inputQueue = this._inputQueue;
            var period = checkPeriod;
            var multipartCommands = this._multipartCommands;
            var data;
            var cmd;
            var body;
            var parts;
            if (_.random(0, freezeFactor) === 0) {
                period += inputDelay;
                console.log("Connection input freezed");
            }
            this._checkInputId = window.setTimeout(this._checkInput, period);
            while (!_.isEmpty(inputQueue)) {
                data = inputQueue.shift();
                cmd = data[0];
                body = data[1];
                if (cmd === "begin-multipart") {
                    cmd = body;
                    multipartCommands[cmd] = [];
                    continue;
                }
                if (multipartCommands[cmd]) {
                    multipartCommands[cmd].push(body);
                    continue;
                }
                if (cmd === "end-multipart") {
                    cmd = body;
                    body = multipartCommands[cmd];
                    delete multipartCommands[cmd];
                    if (!body || _.isEmpty(body)) continue;
                }
                if (cmd === "error") {
                    this.lastError = body;
                }
                if (_.contains(cmd, ".")) {
                    parts = cmd.split(".");
                    this.trigger(parts[0], {cmd: parts[1], body: body});
                }
                this.trigger(cmd, body);
            }
        }
    });
    return Connection;
});

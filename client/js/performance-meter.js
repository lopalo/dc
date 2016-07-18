
define(function (require) {

    var enabled = require("json!settings.json")["enable-performance-meter"];

    function DummyPerformanceMeter() {}
    DummyPerformanceMeter.prototype = {
        constructor: DummyPerformanceMeter,
        init: function () {},
        start: function () {},
        destroy: function () {},
        renderTick: function () {},
    };

    if (!enabled) {
        return DummyPerformanceMeter;
    }

    var FPSMeter = require("fpsmeter");

    function PerformanceMeter(element, connection) {
        this._el = element;
        this._connection = connection;
    }
    PerformanceMeter.prototype = {
        constructor: PerformanceMeter,
        init: function () {
            this._fpsMeter = new FPSMeter(this._el.find("#fps-meter")[0], {
                position: "relative",
                toggleOn: null,
                interval: 350,
                show: "fps",
                graph: 1,
                heat: 1,
                decimals: 0,
                theme: "transparent"
            });
            this._pingMeter = new FPSMeter(this._el.find("#ping-meter")[0], {
                position: "relative",
                toggleOn: null,
                interval: 350,
                threshold: 1000,
                smoothing: 1,
                maxFps: 20,
                show: "ms",
                graph: 1,
                heat: 1,
                decimals: 0,
                theme: "transparent"
            });
        },
        start: function () {
            var self = this;
            function step() {
                if (self.connection === null) return;
                self._pingMeter.tick();
                self._connection.request("area.echo", "ping", step);
            }
            step();
        },
        destroy: function () {
            this._connection = null;
            this._fpsMeter.destroy();
            this._pingMeter.destroy();
        },
        renderTick: function () {
            this._fpsMeter.tick();
        },
    };
    return PerformanceMeter;
});

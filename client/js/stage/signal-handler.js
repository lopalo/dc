
define(function (require) {
    var _ = require("underscore");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");
    var pixi = require("pixi");

    function SignalHandler(options) {
        this._addEffect = options.addEffect;
        this._getObjectModel = options.getObjectModel;
    }
    SignalHandler.prototype = {
        constructor: SignalHandler,
        deleteStageObject: function (ident) {
        },
        process: function (signals) {
            _.each(signals, function (signal) {
                var handler = this["handle" + signal.tag];
                if (!handler) return;
                handler.call(this, signal);
            }, this);

        },
        handleShot: function (signal) {
            var start = Victor.fromArray(signal.shooterPos);
            var end = Victor.fromArray(signal.targetPos);
            var delta = end.subtract(start);
            var tween;

            var graphics = new pixi.Graphics();
            graphics.lineStyle(3, 0x1C8BF7, 0.5);
            graphics.beginFill(0xFFFFFF, 2);
            graphics.drawRoundedRect(0, 0, delta.length(), 1, 0.8);
            graphics.endFill();
            graphics.x = start.x;
            graphics.y = -start.y;
            graphics.rotation = -delta.angle();

            tween = TweenLite.to(graphics, 1, {alpha: 0});
            this._addEffect(graphics, tween);
        }
    };
    return SignalHandler;
});

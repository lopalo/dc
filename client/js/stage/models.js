
define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var utils = require("utils");

    var Camera;
    var StageObject;


    Camera = Backbone.Model.extend({
        defaults: {
            pos: [0, 0],
            height: 0,
            width: 0
        },
        focusTo: function (pos) {
            var shift = new Victor(this.get("width"), this.get("height"))
                            .divide(new Victor(2, 2));
            this.set("pos", pos.subtract(shift).toArray());
        },
        move: function (delta) {
            var pos = Victor.fromArray(this.get("pos"));
            this.set("pos", pos.add(delta).toArray());
        }
    });


    StageObject = Backbone.Model.extend({
        objectType: "nothing",
        proxyAttributes: [
            "objectType",
            "getInfoForUI"
        ],
        defaults: function () {
            return {
                id: "",
                name: "",
                pos: [0, 0],
                angle: 0,
                size: [50, 50],
                actions: []
            };
        },
        getInfoForUI: function () {
            return {};
        },
        applyActions: function (timestamp) {
            _.each(this.get("actions"),
                   this._applyAction.bind(this, timestamp));
        },
        _applyAction: function (timestamp, action) {
            switch (action.tag) {
                case "MoveDistance":
                    this._applyMoveDistance(timestamp, action);
                    break;
                case "EternalRotation":
                    break;
                default:
                    console.log("Unknown action " + action.tag);
            }
        },
        _getT: function (timestamp, a) {
            return (timestamp - a.startTs) / (a.endTs - a.startTs);
        },
        _applyMoveDistance: function (timestamp, a) {
            if (timestamp >= a.endTs) {
                this.set("pos", a.endPos);
                return;
            }
            var t = this._getT(timestamp, a);
            var startPos = Victor.fromArray(a.startPos);
            var endPos = Victor.fromArray(a.endPos);
            var pos = endPos
                      .subtract(startPos)
                      .multiply(new Victor(t, t))
                      .add(startPos)
                      .unfloat();
            this.set("pos", pos.toArray());
        },
    });

    return {
        Camera: Camera,
        StageObject: StageObject,
    };
});

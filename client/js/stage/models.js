
define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");

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
        silent: true,
        set: function (key, val) {
            var attrs;
            if (typeof key === 'object') {
                attrs = key;
            } else {
                attrs = {};
                attrs[key] = val;
            }
            _.extend(this.attributes, attrs);
            return this;
        },
        defaults: function () {
            return {
                id: "",
                name: "",
                pos: [0, 0],
                angle: 0,
                size: [50, 50],
                actions: [],
                "disappearance-reason": null
            };
        },
        getInfoForUI: function () {
            return {};
        },
        applyActions: function (timestamp) {
            _.each(this.get("actions"), function (action) {
                   this._applyAction(action, timestamp);
            }, this);
        },
        _applyAction: function (action, timestamp) {
            switch (action.tag) {
                case "MoveDistance":
                    this._applyMoveDistance(action, timestamp);
                    break;
                case "EternalRotation":
                    this._applyEternalRotation(action, timestamp);
                    break;
                default:
                    console.log("Unknown action " + action.tag);
            }
        },
        _getT: function (a, timestamp) {
            return (timestamp - a.startTs) / (a.endTs - a.startTs);
        },
        _getSecondsDelta: function (a, timestamp) {
            return (timestamp - a.startTs) / 1000;
        },
        _applyMoveDistance: function (a, timestamp) {
            if (timestamp >= a.endTs) {
                this.set("pos", a.endPos);
                return;
            }
            var t = this._getT(a, timestamp);
            var startPos = Victor.fromArray(a.startPos);
            var endPos = Victor.fromArray(a.endPos);
            var pos = endPos
                      .subtract(startPos)
                      .multiply(new Victor(t, t))
                      .add(startPos)
                      .unfloat();
            this.set("pos", pos.toArray());
        },
        _applyEternalRotation: function (a, timestamp) {
            var secondsDelta = this._getSecondsDelta(a, timestamp);
            var newAngle = (a.startAngle + a.rotSpeed * secondsDelta) % 360;
            this.set("angle", newAngle);
        }
    });

    return {
        Camera: Camera,
        StageObject: StageObject,
    };
});

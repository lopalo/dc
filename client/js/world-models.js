
var Area;
var Camera;
var WorldObject;
var Unit;
var CAMERA_SPEED = 10; //px per second

Area = Backbone.Model.extend({
    defaults: {
        id: null,
        background: null
    }
});

Camera = Backbone.Model.extend({
    defaults: {
        x: 0,
        y: 0,
        height: 0,
        width: 0
    },
    moveTo: function (direction) {
        var s = CAMERA_SPEED;
        var v = direction
               .multiply(new Victor(s, s))
               .add(Victor.fromObject(this.attributes));
        this.set(v.toObject());
    }
});


WorldObject = Backbone.Model.extend({
    defaults: function () {
        return {
            pos: [0, 0],
            height: 80,
            width: 40,
            angle: 0
        };
    },
});


Unit = WorldObject.extend({
    defaults: function () {
        var defaults = {
            name: "unit",
            actions: [],
        };
        return _.extend(defaults, WorldObject.prototype.defaults.call(this));
    },
    applyActions: function (timestamp) {
        _.each(this.get("actions"), function(action) {
            switch (action.tag) {
                case "MoveDistance":
                    this._applyMoveDistance(timestamp, action);
                    break;
                default:
                    throw "Unknown action " + action.tag;
            }
        }, this);
    },
    _applyMoveDistance: function (timestamp, a) {
        if (timestamp >= a.endTs) {
            this.set("pos", a.to);
            return;
        }
        var f = (timestamp - a.startTs) / (a.endTs - a.startTs);
        var fromPos = Victor.fromArray(a.from);
        var toPos = Victor.fromArray(a.to);
        var pos = toPos
                  .subtract(fromPos)
                  .multiply(new Victor(f, f))
                  .add(fromPos)
                  .unfloat();
        this.set("pos", pos.toArray());
    }
});



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
        //TODO: use vectors
        this.set({
            x: this.get("x") + direction[0] * CAMERA_SPEED,
            y: this.get("y") + direction[1] * CAMERA_SPEED
        });
    }
});


WorldObject = Backbone.Model.extend({
    defaults: {
        pos: [0, 0],
        height: 80,
        width: 40,
        angle: 0
    },
});


Unit = WorldObject.extend({
    defaults: _.extend({
        name: "unit",
        actions: [],
    }, WorldObject.prototype.defaults),
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
        var pos = toPos.subtract(fromPos)
                       .multiply(Victor(f, f))
                       .add(fromPos);
        //TODO: round a pos to integer (.unfloat())
        this.set("pos", pos.toArray());
    }
});


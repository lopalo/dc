
var Camera;
var WorldObject;
var Unit;

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
        var pos = delta.add(Victor.fromArray(this.get("pos")));
        this.set("pos", pos.toArray());
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


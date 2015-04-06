
define(["underscore", "backbone", "victor"], function(_, Backbone, Victor) {
    var Camera;
    var WorldObject;
    var User;

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
                id: "",
                pos: [0, 0],
                height: 80,
                width: 40,
                angle: 0
            };
        },
    });


    User = WorldObject.extend({
        defaults: function () {
            var defaults = {
                name: "",
                actions: [],
            };
            return _.extend(defaults, WorldObject.prototype.defaults.call(this));
        },
        applyActions: function (timestamp) {
            _.each(this.get("actions"), function(action) {
                switch (action.tag) {
                    case "MoveRoute":
                        this._applyMoveRoute(timestamp, action);
                        break;
                    case "MoveDistance":
                        this._applyMoveDistance(timestamp, action);
                        break;
                    default:
                        console.log("Unknown action " + action.tag);
                }
            }, this);
        },
        _getT: function(timestamp, a) {
            return (timestamp - a.startTs) / (a.endTs - a.startTs);
        },
        _applyMoveRoute: function (timestamp, a) {
            if (timestamp >= a.endTs) {
                this.set("pos", _.last(a.positions));
                return;
            }
            var t = this._getT(timestamp, a);
            var tv = new Victor(t, t);
            function reduce(points) {
                if (points.length === 2) {
                    var vect = points[1].subtract(points[0]);
                    var angle = vect.angleDeg();
                    var pos = vect.multiply(tv).add(points[0]).unfloat();
                    return {pos: pos.toArray(), angle: angle};
                }
                var prev = _.head(points);
                var newPoints = [];
                _.each(_.tail(points), function (point) {
                    newPoints.push(point.clone().subtract(prev)
                                   .multiply(tv).add(prev));
                    prev = point;
                });
                return reduce(newPoints);
            }
            var res = reduce(_.map(a.positions, Victor.fromArray));
            this.set(res);
        },
        _applyMoveDistance: function (timestamp, a) {
            if (timestamp >= a.endTs) {
                this.set("pos", a.to);
                return;
            }
            var t = this._getT(timestamp, a);
            var fromPos = Victor.fromArray(a.from);
            var toPos = Victor.fromArray(a.to);
            var pos = toPos
                      .subtract(fromPos)
                      .multiply(new Victor(t, t))
                      .add(fromPos)
                      .unfloat();
            this.set("pos", pos.toArray());
        }
    });
    return {
        Camera: Camera,
        User: User
    };
});

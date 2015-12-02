
define(function(require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var utils = require("utils");

    var Camera;
    var StageObject;
    var User;
    var Gate;
    var Asteroid;

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
                height: 50,
                width: 50,
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
        _getT: function(timestamp, a) {
            return (timestamp - a.startTs) / (a.endTs - a.startTs);
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
        },
    });


    User = StageObject.extend({
        objectType: "user",
        defaults: function () {
            var defaults = {
                height: 40,
                width: 80,
            };
            return _.extend(User.__super__.defaults.call(this), defaults);
        },
        getInfoForUI: function () {
            return {
                name: this.get("name"),
                durability: this.get("durability"),
                speed: this.get("speed")
            };
        },
        _applyAction: function (timestamp, action) {
            switch (action.tag) {
                case "MoveRoute":
                    this._applyMoveRoute(timestamp, action);
                    break;
                default:
                    User.__super__._applyAction.call(this, timestamp, action);
            }
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
    });


    Gate = StageObject.extend({
        objectType: "gate",
        defaults: function () {
            var defaults = {
                height: 300, //FIXME
                width: 300,
            };
            return _.extend(Gate.__super__.defaults.call(this), defaults);
        },
        getInfoForUI: function () {
            return {
                name: this.get("name"),
            };
        },
    });

    Asteroid = StageObject.extend({
        objectType: "asteroid",
        defaults: function () {
            var size = utils.randInt(70, 200);
            var defaults = {
                height: size, //FIXME
                width: size,
            };
            return _.extend(Asteroid.__super__.defaults.call(this), defaults);
        },
        getInfoForUI: function () {
            return {
                name: this.get("name"),
            };
        },
        _applyAction: function (timestamp, action) {
            switch (action.tag) {
                case "MoveCircularTrajectory":
                    break;
                default:
                    Asteroid.__super__._applyAction.call(this,
                                                         timestamp, action);
            }
        },
    });


    return {
        Camera: Camera,
        User: User,
        Gate: Gate,
        Asteroid: Asteroid
    };
});

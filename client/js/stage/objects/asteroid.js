define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");
    var pixi = require("pixi");

    var models = require("stage/models");
    var views = require("stage/views");

    var Asteroid;
    var AsteroidView;


    Asteroid = models.StageObject.extend({
        objectType: "asteroid",
        defaults: function () {
            var defaults = {
                "max-durability": 0,
                durability: 0,
            };
            return _.extend(Asteroid.__super__.defaults.call(this), defaults);
        },

        getInfoForUI: function () {
            var pullAllowed = !_.find(this.get("actions"), function (i) {
                return i.tag === "MoveCircularTrajectory";
            });
            return {
                name: this.get("name"),
                durability: [this.get("durability"),
                             this.get("max-durability")],
                pullAllowed: pullAllowed
            };
        },
        _applyAction: function (action, timestamp) {
            switch (action.tag) {
                case "MoveCircularTrajectory":
                    this._applyMoveCircularTrajectory(action, timestamp);
                    break;
                default:
                    Asteroid.__super__._applyAction.call(this, action,
                                                         timestamp);
            }
        },
        _applyMoveCircularTrajectory: function (a, timestamp) {
            var secondsDelta = this._getSecondsDelta(a, timestamp);
            var angle = (a.startAngle + a.rotSpeed * secondsDelta) % 360;
            var pos = new Victor(1, 0)
                        .rotateDeg(angle)
                        .multiply(new Victor(a.radius, a.radius))
                        .add(Victor.fromArray(a.center));
            this.set("pos", pos.toArray());
        }
    });


    AsteroidView = views.StageObject.extend({
        showName: false,
        _getTexturePath: function () {
            return "img/" + this._model.get("asset") + ".png";
        },
    });

    return {
        Asteroid: Asteroid,
        AsteroidView: AsteroidView
    };

});


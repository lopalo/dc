define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");
    var pixi = require("pixi");

    var utils = require("utils");
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


    AsteroidView = views.StageObject.extend({
        texturePath: "img/asteroid.png",
        showName: false
    });

    return {
        Asteroid: Asteroid,
        AsteroidView: AsteroidView
    };

});


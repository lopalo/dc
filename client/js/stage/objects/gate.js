
define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");
    var pixi = require("pixi");

    var models = require("stage/models");
    var views = require("stage/views");

    var Gate;
    var GateView;

    Gate = models.StageObject.extend({
        objectType: "gate",
        getInfoForUI: function () {
            return {
                name: this.get("name"),
            };
        },
    });


    GateView = views.StageObject.extend({
        texturePath: "img/gate.png",
    });

    return {
        Gate: Gate,
        GateView: GateView
    };

});


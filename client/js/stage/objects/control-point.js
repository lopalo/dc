
define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");
    var pixi = require("pixi");

    var models = require("stage/models");
    var views = require("stage/views");

    var ControlPoint;
    var ControlPointView;


    ControlPoint = models.StageObject.extend({
        objectType: "control-point",
        defaults: function () {
            var defaults = {
                "max-durability": 0,
                durability: 0,
                owner: null,
            };
            return _.extend(
                ControlPoint.__super__.defaults.call(this),
                defaults
            );
        },

        getInfoForUI: function () {
            return {
                name: this.get("name"),
                durability: [this.get("durability"),
                             this.get("max-durability")],
                owner: this.get("owner")
            };
        },
    });


    ControlPointView = views.StageObject.extend({
        texturePath: "img/space_station_1.png",
        capturedTexturePath: "img/space_station_2.png",
        initialize: function (options) {
            ControlPointView.__super__.initialize.call(this, options);
            this.listenTo(this._model, "change:owner", this._changeTexture);
        },
        _getTexturePath: function () {
            if (this._model.get("owner") === null) {
                return this.texturePath;
            } else {
                return this.capturedTexturePath;
            }
        },
        _changeTexture: function () {
            var texture = pixi.Texture.fromImage(this._getTexturePath());
            this._sprite.texture = texture;
        }
    });

    return {
        ControlPoint: ControlPoint,
        ControlPointView: ControlPointView
    };

});


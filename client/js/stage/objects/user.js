
define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");
    var pixi = require("pixi");

    var models = require("stage/models");
    var views = require("stage/views");

    var User;
    var UserView;

    User = models.StageObject.extend({
        objectType: "user",


        defaults: function () {
            var defaults = {
                speed: 0,
                kills: 0,
                deaths: 0,
                "max-durability": 0,
                durability: 0,
                "area-exiting": false
            };
            return _.extend(User.__super__.defaults.call(this), defaults);
        },
        getInfoForUI: function () {
            var names = ["name", "speed", "kills", "deaths"];
            var info = _.pick(this.attributes, names);
            info.durability = [this.get("durability"),
                               this.get("max-durability")];
            return info;

        },
        _applyAction: function (timestamp, action) {
            switch (action.tag) {
                case "MoveRoute":
                    this._applyMoveRoute(timestamp, action);
                    break;
                case "Rotation":
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


    UserView = views.StageObject.extend({
        texturePath: "img/ship.png",
        initialize: function (options) {
            UserView.__super__.initialize.call(this, options);
            this._isSelf = options.isSelf;
            this._appearanceReason = options.reason;
            this._updateAllowed = true;

        },
        update: function () {
            if (!this._updateAllowed) return;
            UserView.__super__.update.call(this);
        },
        _getTextColor: function () {
            return this._isSelf ? "#8B8FBD" : "white";
        },
        _appearanceEffect: function () {
            var self = this;
            var size = this._model.get("size");
            var width = size[0];
            var height = size[1];
            var sprite = this._sprite;
            var onComplete = function () {
                self._updateAllowed = true;
                self.update();
            };
            var rotation = this._getRotation();
            var toProps;
            var tween;
            switch (this._appearanceReason) {
                case "LogIn":
                    sprite.width = sprite.height = 1;
                    sprite.rotation = rotation - 4 * Math.PI;
                    toProps = {rotation: rotation, width: width, height: height};
                    tween = TweenLite.to(sprite, 1, toProps);
                    break;
                case "Entry":
                    sprite.width = 50 * width;
                    sprite.height = 0.01 * height;
                    tween = TweenLite.to(sprite, 0.5,
                                        {width: width, height: height});
                    break;
                case "Recovery":
                    tween = UserView.__super__._appearanceEffect.call(this);
                    break;
                default:
                    tween = UserView.__super__._appearanceEffect.call(this);

            }
            this._updateAllowed = false;
            tween.eventCallback("onComplete", onComplete);
            return tween;
        },
        _getDisappearanceReason: function () {
            if (this._model.get("area-exiting")) return "Exit";
            return UserView.__super__._getDisappearanceReason.call(this);
        },
        _disappearanceEffect: function (reason, sprite) {
            var size = this._model.get("size");
            var width = size[0];
            var height = size[1];
            var rotation = this._getRotation();
            var toProps;
            var tween;
            switch (reason) {
                case "Destruction":
                    toProps = {width: 2 * width, height: 2 * height, alpha: 0};
                    tween = TweenLite.to(sprite, 0.5, toProps);
                    break;
                case "Exit":
                    toProps = {width: 50 * width, height: 0.01 * height};
                    tween = TweenLite.to(sprite, 0.5, toProps);
                    break;
                case "LogOut":
                    toProps = {
                        rotation: rotation - 4 * Math.PI,
                        width: 1,
                        height: 1
                    };
                    tween = TweenLite.to(sprite, 1, toProps);
                    break;
                default:
                    tween = UserView.__super__._disappearanceEffect.call(
                        this, reason, sprite
                    );
            }
            return tween;
        }

    });

    return {
        User: User,
        UserView: UserView
    };

});


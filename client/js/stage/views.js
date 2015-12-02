
define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");
    var pixi = require("pixi");


    var Layer;
    var ObjectLayer;
    var Background;
    var Midground;
    var StageObject;
    var User;
    var Gate;
    var Asteroid;


    function StageView(options) {
        this.cid = _.uniqueId('view');
        this._container = null;
        this._containerForListening = null;
        this.initialize(options);

    }
    StageView.extend = Backbone.View.extend;
    StageView.prototype = Object.create(Backbone.Events);
    _.extend(StageView.prototype, {
        constructor: StageView,
        initialize: function () {},

        events: {},

        createContainer: function (parent) {
            this._createContainer();
            this._delegateEvents();
            parent.addChild(this._container);
        },
        destroy: function () {
            this.stopListening();
            this._undelegateEvents();
            this._container.parent.removeChild(this._container);
            this._container = null;
            this._containerForListening = null;
        },
        addView: function (view) {
            view.createContainer(this._container);
        },
        _getContainerForListening: function () {
            return this._containerForListening || this._container;
        },
        _createContainer: function () {},
        _delegateEvents: function () {
            var container = this._getContainerForListening();
            _.each(this.events, function (method, callback) {
                container[callback] = this[method].bind(this);
            }, this);
        },
        _undelegateEvents: function () {
            var container = this._getContainerForListening();
            _.each(this.events, function (method, callback) {
                container[callback] = null;
            });
        },
    });



    Background = StageView.extend({
        events: {
            click: "_click",
            mousedown: "_mouseDown",
            mouseup: "_mouseUp",
            mousemove: "_mouseMove"
        },
        initialize: function (options) {
            Background.__super__.initialize.call(this, options);
            this._area = options.area;
            this._camera = options.camera;

            this.listenTo(this._area, "change:background",
                                    this._changeBackground);
            this.listenTo(this._camera, "change:width change:height",
                                            this.resize);
        },
        resize: function () {
            var container = this._container;
            var width = this._camera.get("width");
            var height = this._camera.get("height");
            var scale;
            container.scale.x = container.scale.y = 1;
            scale = Math.max(width / container.width,
                             height / container.height);
            container.scale.x = container.scale.y = scale;
        },
        _createContainer: function () {
            var bg = "img/" + this._area.get("background");
            this._container = pixi.Sprite.fromImage(bg);
            this._container.interactive = true;
            this.resize();
        },
        _changeBackground: function () {
            var bg = "img/" + this._area.get("background");
            this._container.texture = pixi.Texture.fromImage(bg);
            this.resize();
        },
        _click: function () {
            this.trigger("click");
        },
        _mouseDown: function () {
            this.trigger("mouseDown");
        },
        _mouseUp: function () {
            this.trigger("mouseUp");
        },
        _mouseMove: function (ev) {
            if (ev.data.originalEvent.which !== 1) return;
            this.trigger("mouseMove", Victor.fromObject(ev.data.global));
        }
    });


    Layer = StageView.extend({
        initialize: function (options) {
            this._parallaxIndex = options.parallaxIndex;
            this._camera = options.camera;
            this.listenTo(this._camera, "change:pos", this._move);
        },
        _move: function () {
            var camera = this._camera;
            var idx = this._parallaxIndex;
            var d = Victor.fromArray(camera.get("pos"))
                    .subtract(Victor.fromArray(camera.previous("pos")))
                    .multiply(new Victor(idx, idx));
            this._updatePos(d);
        },
        _updatePos: function () {}
    });


    Midground = Layer.extend({
        texturePath: "img/midground.png",
        initialize: function (options) {
            Midground.__super__.initialize.call(this, options);
            this._camera = options.camera;
            this.listenTo(this._camera, "change:width change:height",
                                            this._resize);
        },
        _createContainer: function () {
            this._container = pixi.extras.TilingSprite
                              .fromImage(this.texturePath);
        },
        _updatePos: function (delta) {
            this._container.tilePosition.x -= delta.x;
            this._container.tilePosition.y -= delta.y;
        },
        _resize: function () {
            var container = this._container;
            container.width = this._camera.get("width");
            container.height = this._camera.get("height");
        },
    });


    ObjectLayer = Layer.extend({
        //TODO: culling
        addEffect: function (effectContainer, tween) {
            var container = this._container;
            var onComplete = function () {
                container.removeChild(effectContainer);
            };
            container.addChildAt(effectContainer, 0); //FIXME: it's not effecient
            tween.eventCallback("onComplete", onComplete);
        },
        _createContainer: function () {
            this._container = new pixi.Container();
        },
        _updatePos: function (delta) {
            this._container.x -= delta.x;
            this._container.y -= delta.y;
        },

    });


    StageObject = StageView.extend({
        events: {
            click: "_click",
            mouseup: "_mouseUp",
            mouseover: "_mouseOver",
            mouseout: "_mouseOut"
        },
        texturePath: "", //TODO: use horizontal sprites
        showName: true,
        initialize: function (options) {
            this._model = options.model;
            this._sprite = null;
            this.listenTo(this._model, "change", this._update);
        },
        destroy: function (reason) {
            var pos = Victor.fromArray(this._model.get("pos"));
            var parent = this._container.parent;
            var sprite = this._sprite;
            var onComplete = function () {
                parent.removeChild(sprite);
            };
            var tween;

            this._sprite = null;
            StageObject.__super__.destroy.call(this);
            parent.addChild(sprite);
            sprite.x = pos.x;
            sprite.y = pos.y;
            tween = this._disappearanceEffect(reason, sprite);
            tween.eventCallback("onComplete", onComplete);
        },
        _createContainer: function () {
            var model = this._model;
            var width = model.get("width");
            var height = model.get("height");
            var name = model.get("name");
            var container;
            var sprite;
            var text;

            this._container = container = new pixi.Container();
            this._sprite = sprite = pixi.Sprite.fromImage(this.texturePath);
            this._containerForListening = sprite;
            container.addChild(sprite);
            sprite.width = width;
            sprite.height = height;
            sprite.anchor.x = sprite.anchor.y = 0.5;
            sprite.interactive = true;
            if (name && this.showName) {
                text = new pixi.Text(name, {fill: this._getTextColor(),
                                            font: "14px Arial"});
                text.anchor.x = text.anchor.y = 0.5;
                text.y = -height / 1.5;
                container.addChild(text);
            }
            this._update();
            this._appearanceEffect();
        },
        _update: function () {
            var model = this._model;
            var container = this._container;
            var pos = Victor.fromArray(model.get("pos"));
            container.x = pos.x;
            container.y = pos.y;
            this._sprite.rotation = this._getRotation();
        },
        _getRotation: function () {
            return (this._model.get("angle") - 90) * (Math.PI / 180); //TODO: use horizontal sprites
        },
        _getTextColor: function () {
            return "white";
        },
        _appearanceEffect: function () {
            this._sprite.alpha = 0;
            return TweenLite.to(this._sprite, 1, {alpha: 1});
        },
        _disappearanceEffect: function (reason, sprite) {
            return TweenLite.to(sprite, 1, {alpha: 0});
        },
        _click: function () {
            this.trigger("click", this._model.get("id"));
        },
        _mouseUp: function () {
            this.trigger("mouseUp", this._model.get("id"));
        },
        _mouseOver: function () {
            this.trigger("mouseOver", this._model.get("id"));
        },
        _mouseOut: function () {
            this.trigger("mouseOut", this._model.get("id"));
        }
    });


    User = StageObject.extend({
        texturePath: "img/ship.png",
        initialize: function (options) {
            User.__super__.initialize.call(this, options);
            this._isSelf = options.isSelf;
            this._appearanceReason = options.reason;
            this._updateAllowed = true;

        },
        _update: function () {
            if (!this._updateAllowed) return;
            User.__super__._update.call(this);
        },
        _getTextColor: function () {
            return this._isSelf ? "#8B8FBD" : "white";
        },
        _appearanceEffect: function () {
            var self = this;
            var model = this._model;
            var width = model.get("width");
            var height = model.get("height");
            var sprite = this._sprite;
            var onComplete = function () {
                self._updateAllowed = true;
                self._update();
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
                    sprite.width = 0.01 * width;
                    sprite.height = 50 * height;
                    tween = TweenLite.to(sprite, 0.5,
                                        {width: width, height: height});
                    break;

                case "Recovery":
                    tween = User.__super__._appearanceEffect.call(this);
                    break;
                default:
                    tween = User.__super__._appearanceEffect.call(this);

            }
            this._updateAllowed = false;
            tween.eventCallback("onComplete", onComplete);
            return tween;
        },
        _disappearanceEffect: function (reason, sprite) {
            var model = this._model;
            var width = model.get("width");
            var height = model.get("height");
            var rotation = this._getRotation();
            var toProps;
            var tween;
            switch (reason) {
                case "Burst":
                    toProps = {width: 2 * width, height: 2 * height, alpha: 0};
                    tween = TweenLite.to(sprite, 0.5, toProps);
                    break;
                case "Exit":
                    toProps = {width: 0.01 * width, height: 50 * height};
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
                    tween = User.__super__._disappearanceEffect.call(
                        this, reason, sprite
                    );
            }
            return tween;
        }

    });

    Gate = StageObject.extend({
        texturePath: "img/gate.png",
    });

    Asteroid = StageObject.extend({
        texturePath: "img/asteroid.png",
        showName: false
    });


    return {
        Background: Background,
        Midground: Midground,
        ObjectLayer: ObjectLayer,
        User: User,
        Gate: Gate,
        Asteroid: Asteroid
    };

});


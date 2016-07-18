
define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");
    var pixi = require("pixi");
    var settings = require("json!settings.json");

    var Layer;
    var ObjectLayer;
    var Background;
    var Midground;
    var StageObject;


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
            mousedown: "_pointerDown",
            mouseup: "_pointerUp",
            mousemove: "_pointerMove",

            tap: "_click",
            touchstart: "_pointerDown",
            touchend: "_pointerUp",
            touchmove: "_pointerMove"
        },
        initialize: function (options) {
            Background.__super__.initialize.call(this, options);
            this._area = options.area;
            this._camera = options.camera;

            this.listenTo(this._area, "change:areaId", this._changeBackground);
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
            this._container = new pixi.Sprite();
            this._container.interactive = true;
            this.resize();
        },
        _changeBackground: function () {
            var areaId = this._area.get("areaId");
            var bg = "/client/img/" + settings.areas[areaId].background;
            this._container.texture = pixi.Texture.fromImage(bg);
            this.resize();
        },
        _click: function (ev) {
            this.trigger("click", Victor.fromObject(ev.data.global));
        },
        _pointerDown: function () {
            this.trigger("pointerDown");
        },
        _pointerUp: function () {
            this.trigger("pointerUp");
        },
        _pointerMove: function (ev) {
            if (ev.type === "touchmove" || ev.data.originalEvent.which === 1) {
                this.trigger("pointerMove", Victor.fromObject(ev.data.global));
            }
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
        texturePath: "/client/img/midground.png",
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
        addEffect: function (effectContainer, tween) {
            var container = this._container;
            var onComplete = function () {
                container.removeChild(effectContainer);
            };
            container.addChildAt(effectContainer, 0); //FIXME: it's not effecient
            tween.eventCallback("onComplete", onComplete);
            if (document.hidden) {
                tween.progress(1);
            }

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
            mouseup: "_pointerUp",
            mouseover: "_pointerOver",
            mouseout: "_pointerOut",

            tap: "_click",
            touchstart: "_pointerOver"
        },
        texturePath: "",
        showName: true,
        initialize: function (options) {
            this._model = options.model;
            this._sprite = null;
        },
        destroy: function () {
            var reason = this._getDisappearanceReason();
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
            sprite.y = -pos.y;
            tween = this._disappearanceEffect(reason, sprite);
            tween.eventCallback("onComplete", onComplete);
            if (document.hidden) {
                tween.progress(1);
            }
        },
        _getDisappearanceReason: function () {
            return this._model.get("disappearance-reason");
        },
        _getTexturePath: function () {
            return this.texturePath;
        },
        _createContainer: function () {
            var model = this._model;
            var size = model.get("size");
            var name = model.get("name");
            var texturePath = this._getTexturePath();
            var container;
            var sprite;
            var text;
            var tween;

            this._container = container = new pixi.Container();
            this._sprite = sprite = pixi.Sprite.fromImage(texturePath);
            this._containerForListening = sprite;
            container.addChild(sprite);
            sprite.width = size[0];
            sprite.height = size[1];
            sprite.anchor.x = sprite.anchor.y = 0.5;
            sprite.interactive = true;
            if (name && this.showName) {
                text = new pixi.Text(name, {fill: this._getTextColor(),
                                            font: "14px Arial"});
                text.anchor.x = text.anchor.y = 0.5;
                text.y = -(_.max(size) / 2 + 15);
                container.addChild(text);
            }
            this.update();
            tween = this._appearanceEffect();
            if (document.hidden) {
                tween.progress(1);
            }

        },
        update: function () {
            var model = this._model;
            var container = this._container;
            var pos = Victor.fromArray(model.get("pos"));
            container.x = pos.x;
            container.y = -pos.y;
            this._sprite.rotation = this._getRotation();
        },
        _getRotation: function () {
            return -this._model.get("angle") * (Math.PI / 180);
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
        _pointerUp: function () {
            this.trigger("pointerUp", this._model.get("id"));
        },
        _pointerOver: function () {
            this.trigger("pointerOver", this._model.get("id"));
        },
        _pointerOut: function () {
            this.trigger("pointerOut", this._model.get("id"));
        }
    });

    return {
        Background: Background,
        Midground: Midground,
        ObjectLayer: ObjectLayer,
        StageObject: StageObject
    };

});


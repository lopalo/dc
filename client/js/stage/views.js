
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
            if (this._container === null) return;
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
        _mouseDown: function () {
            this.trigger("mouseDown");
        },
        _mouseUp: function () {
            this.trigger("mouseUp");
        },
        _mouseMove: function (data) {
            if (data.data.originalEvent.which !== 1) return;
            this.trigger("mouseMove", Victor.fromObject(data.data.global));
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

    });


    ObjectLayer = Layer.extend({
        _createContainer: function () {
            this._container = new pixi.Container();
        },
        _updatePos: function (delta) {
            this._container.x -= delta.x;
            this._container.y -= delta.y;
        },
    });


    StageObject = StageView.extend({
        texturePath: "",
        initialize: function (options) {
            this._model = options.model;
            this._sprite = null;
            this.listenTo(this._model, "change", this._update);
        },
        destroy: function (reason) {
            StageObject.__super__.destroy.call(this);
            //TODO: effect
        },
        _createContainer: function () {
            var model = this._model;
            var width = model.get("width");
            var height = model.get("height");
            var name = model.get("name");
            var sprite;
            var container;

            this._container = container = new pixi.Container();
            this._sprite = sprite = pixi.Sprite.fromImage(this.texturePath);
            this._containerForListening = sprite;
            container.addChild(sprite);
            sprite.width = width;
            sprite.height = height;
            sprite.anchor.x = sprite.anchor.y = 0.5;
            sprite.interactive = true;
            if (name) {
                //TODO: fix position and size of the text
                container.addChild(new pixi.Text(name, {fill: "white"}));
            }
            this._update();
        },
        _update: function () {
            var model = this._model;
            var container = this._container;
            var pos = Victor.fromArray(model.get("pos"));
            container.position.x = pos.x;
            container.position.y = pos.y;
            this._sprite.rotation = this._getRotation();
        },
        _getRotation: function () {
            return (this._model.get("angle") - 90) * (Math.PI / 180);
        },

    });


    User = StageObject.extend({
        texturePath: "img/ship.png",
    });



    return {
        Background: Background,
        Midground: Midground,
        ObjectLayer: ObjectLayer,
        User: User
    };

});


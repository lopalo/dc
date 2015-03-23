
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");
    require("tween-lite-css");

    var Layer;
    var Background;
    var Midground;
    var ViewPort;
    var UserView;

    Layer = Backbone.View.extend({
        attributes: {
            draggable: false
        },
        className: "world-layer",
        initialize: function (options) {
            this.parallaxIndex = options.parallaxIndex;
            this.listenTo(this.model, "change:pos", this.move);
        },
        move: function () {
            var camera = this.model;
            var idx = this.parallaxIndex;
            var d = Victor.fromArray(camera.get("pos"))
                    .subtract(Victor.fromArray(camera.previous("pos")))
                    .multiply(new Victor(idx, idx));
            this.updatePos(d);
        },
        updatePos: function (delta) {
            var el = this.$el;
            el.css({
                left: parseInt(el.css("left"), 10) - delta.x,
                bottom: parseInt(el.css("bottom"), 10) - delta.y
            });
        }
    });


    var Background = Layer.extend({
        id: "background",
        className: "world-layer world-background",
        events: {
            mousedown: "mouseDown",
            mouseup: "mouseUp",
            mousemove: "mouseMove"
        },
        initialize: function (options) {
            Layer.prototype.initialize.call(this, options);
            this.area = options.area;
            this.ui = options.ui;
            this.listenTo(this.area, "change:background", this.render);
            this.mousePos = null;
            this.route = [];
        },
        render: function () {
            var bg = this.area.get("background");
            this.$el.css({"background-image": "url(img/" + bg + ")"});
        },
        mouseDown: function () {
            this.mousePos = null;
            this.route = [];
        },
        mouseUp: function () {
            if (this.ui.get("controlMode") !== "move") return;
            if (!_.isEmpty(this.route)) {
                this.trigger("move-along-route", this.route);
            }
            this.route = [];
        },
        mouseMove: function (e) {
            if (e.which !== 1) return;
            var height = this.model.get("height");
            var pos = new Victor(e.offsetX, height - e.offsetY);
            switch (this.ui.get("controlMode")) {
                case "view":
                    if (this.mousePos === null) {
                        this.mousePos = pos;
                    }
                    var previousPos = this.mousePos.clone();
                    this.mousePos = pos;
                    this.model.move(previousPos.subtract(pos));
                    break;
                case "move":
                    pos = pos.add(Victor.fromArray(this.model.get("pos")));
                    this.route.push(pos);
                    break;
            }
        }
    });


    var Midground = Layer.extend({
        className: "world-layer world-midground",
        updatePos: function (delta) {
            delta.invertX();
            var el = this.$el;
            var pos = el.css("background-position").split(" ");
            pos = _.map(pos, function (i) {return parseInt(i, 10); });
            pos = Victor.fromArray(pos).add(delta);
            pos = pos.x + "px " + pos.y + "px";
            el.css("background-position", pos);
        }
    });



    ViewPort = Backbone.View.extend({
        initialize: function () {
            _.bindAll(this, "updateCamera");
            $(window).on("resize", this.updateCamera);
            this.updateCamera();
        },
        updateCamera: function () {
            this.model.set("width", this.$el.width());
            this.model.set("height", this.$el.height());
        },
        remove: function () {
            Backbone.View.prototype.remove.call(this);
            $(window).off("resize", this.updateCamera);
        }
    });


    UserView = Backbone.View.extend({
        //TODO: culling in the object layer
        width: 140, //pixels
        labelHeight: 20, //pixels
        className: "world-object text-center",
        events: {
            click: "click",
            mouseenter: "mouseEnter",
            mouseleave: "mouseLeave"
        },
        popoverTemplate: _.template($("#user-popover-template").html()),
        initialize: function (options) {
            this.isSelf = options.isSelf;
            this.appearanceReason = options.reason;
            this.ui = options.ui;
            this.updateAllowed = true;
            this.img = null;
            this.listenTo(this.model, "change", this.update);
        },
        render: function () {
            var el = this.$el;
            var model = this.model;
            var height = model.get("height");
            el.css({width: this.width, height: this.labelHeight + height});
            $("<div></div>").html(model.get("name")).appendTo(el);
            this.img = $("<img>", {src: "img/ship.png"})
                .attr("draggable", false)
                .css({
                    width: model.get("width"),
                    height: height
                })
                .appendTo(el);
            this.update();
            this.appearanceEffect();
            return el;
        },
        rotate: function () {
            var angle = -this.model.get("angle") - 90;
            return "rotate(" + angle + "deg)";
        },
        update: function () {
            if (!this.updateAllowed) return;
            var model = this.model;
            var xShift = this.width / 2;
            var yShift = model.get("height") / 2;
            var pos = Victor.fromArray(model.get("pos"))
                      .subtract(new Victor(xShift, yShift));
            this.$el.css({left: pos.x, bottom: pos.y});
            this.img.css({transform: this.rotate()});
        },
        destroy: function (reason) {
            this.disappearanceEffect(reason);
            this.remove();
        },
        mouseEnter: function () {
            if (this.ui.get("controlMode") !== "view") return;
            var el = this.$el;
            var model = this.model;
            var content = this.popoverTemplate(model.attributes);
            el.popover({
                title: model.get("name"),
                content: content,
                container: el,
                html: true
            });
            el.popover("show");
        },
        mouseLeave: function () {
            this.$el.popover("destroy");
        },
        click: function () {
            if (this.ui.get("controlMode") !== "shot" || this.isSelf) return;
            this.trigger("shot", this.model.get("id"));
        },
        appearanceEffect: function () {
            var self = this;
            var onComplete = function () { self.updateAllowed = true; };
            var img = this.img;
            var rotate = this.rotate();
            var duration = 1;
            var fromProps;
            var toProps;
            switch (this.appearanceReason) {
                case "LogIn":
                    this.updateAllowed = false;
                    fromProps = {rotationY: 90, rotationZ: rotate};
                    toProps = {rotationY: 0};
                    break;
                case "Entry":
                    this.updateAllowed = false;
                    duration = 0.5;
                    fromProps = {scaleX: 0.01, scaleY: 50, rotationZ: rotate};
                    toProps = {scaleX: 1, scaleY: 1, rotationZ: rotate};
                    break;
                default: //Recovery
                    fromProps = {opacity: 0};
                    toProps = {opacity: 1};
            }
            TweenLite.fromTo(img, duration, fromProps, toProps)
                     .eventCallback("onComplete", onComplete);
        },
        disappearanceEffect: function (reason) {
            var el = this.$el;
            var img = this.img.clone();
            var model = this.model;
            var width = model.get("width");
            var height = model.get("height");
            var pos = Victor.fromArray(model.get("pos"))
                      .subtract(new Victor(width / 2, height / 2));
            var onComplete = function () { img.remove(); };
            var duration = 1;
            var toProps;
            img.appendTo(el.parent())
               .css({
                   left: pos.x,
                   bottom: pos.y,
                   position: "absolute",
                   transform: this.rotate(),
               });
            switch (reason) {
                case "Burst":
                    toProps = {scaleX: 2, scaleY: 2, opacity: 0};
                    break;
                case "Exit":
                    duration = 0.5;
                    toProps = {scaleX: 0.01, scaleY: 50};
                    break;
                default: //LogOut
                    toProps = {rotationY: 90};
            }
            TweenLite.to(img, duration, toProps)
                     .eventCallback("onComplete", onComplete);
        }
    });
    return {
        Layer: Layer,
        Background: Background,
        Midground: Midground,
        ViewPort: ViewPort,
        UserView: UserView
    };
});

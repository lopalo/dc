
var Layer;
var Background;
var Midground;
var ViewPort;
var UserView;

Layer = Backbone.View.extend({
    attributes: {
        draggable: false
    },
    className: "layer",
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
    className: "layer background",
    events: {
        click: "click",
        mousedown: "mouseDown",
        mousemove: "mouseMove"
    },
    initialize: function (options) {
        Layer.prototype.initialize.call(this, options);
        this.area = options.area;
        this.ui = options.ui;
        this.listenTo(this.area, "change:background", this.render);
        this.mousePos = null;
    },
    render: function () {
        var bg = this.area.get("background");
        this.$el.css({"background-image": "url(img/" + bg + ")"});
    },
    click: function (e) {
        if (this.ui.get("controlMode") !== "move") return;
        var height = this.model.get("height");
        var pos = new Victor(e.offsetX, height - e.offsetY);
        pos.add(Victor.fromArray(this.model.get("pos")));
        this.trigger("move-to", pos);
    },
    mouseDown: function () {
        this.mousePos = null;
    },
    mouseMove: function (e) {
        if (this.ui.get("controlMode") !== "view") return;
        if (e.which !== 1) return;
        var height = this.model.get("height");
        var pos = new Victor(e.screenX, height - e.screenY);
        if (this.mousePos === null) {
            this.mousePos = pos;
        }
        var previousPos = this.mousePos.clone();
        this.mousePos = pos;
        t = previousPos.subtract(pos);
        this.model.move(t);
    }
});


var Midground = Layer.extend({
    className: "layer midground",
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
        this.listenTo(this.model, "destroy-view", this.destroy);
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
        this.img.css({"-webkit-transform": this.rotate()});
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
        var endEffect = function () {
            img.off("webkitTransitionEnd", endEffect)
               .removeClass("fast-effect");
            self.updateAllowed = true;
        };
        var rotate = this.rotate();
        var img = this.img;
        var fun;
        img.addClass("fast-effect")
           .bind("webkitTransitionEnd", endEffect);
        _.delay(endEffect, 600); // must be synchronized with the transition duration
        //TODO: animation refactoring
        switch (this.appearanceReason) {
            case "LogIn":
                this.updateAllowed = false;
                img.css({"-webkit-transform": rotate + " rotateY(90deg)"});
                fun = function () {
                    img.css({
                        "-webkit-transform": rotate + " rotateY(0deg)",
                    });
                };
                break;
            case "Entry":
                this.updateAllowed = false;
                img.css({"-webkit-transform": rotate + " scale(.01, 20) "});
                fun = function () {
                    img.css({
                        "-webkit-transform": rotate + " scale(1, 1) ",
                    });
                };
                break;
            default: //Recovery
                img.css({opacity: 0});
                fun = function () { img.css({opacity: 1}); };
        }
        _.delay(fun, 100);
    },
    disappearanceEffect: function (reason) {
        var el = this.$el;
        var img = this.img.clone();
        var model = this.model;
        var rotate = this.rotate();
        var width = model.get("width");
        var height = model.get("height");
        var pos = Victor.fromArray(model.get("pos"))
                  .subtract(new Victor(width / 2, height / 2));
        var endEffect = function () { img.remove(); };
        var fun;
        img.appendTo(el.parent())
           .addClass("fast-effect")
           .css({
               left: pos.x,
               bottom: pos.y,
               position: "absolute",
               "-webkit-tranform": rotate,
           })
           .bind("webkitTransitionEnd", endEffect);
        _.delay(endEffect, 600); // must be synchronized with the transition duration
        switch (reason) {
            case "Burst":
                fun = function () {
                    img.css({
                        "-webkit-transform": rotate + " scale(2, 2)",
                        opacity: 0
                    });
                };
                break;
            case "Exit":
                fun = function () {
                    img.css({
                        "-webkit-transform": rotate + " scale(.01, 20)",
                    });
                };
                break;
            default: //LogOut
                fun = function () {
                    img.css({
                        "-webkit-transform": rotate + " rotateY(90deg)",
                    });
                };
        }
        _.delay(fun, 100);
    }
});


var Layer;
var Background;
var Midground;
var ViewPort;
var UnitView;

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


UnitView = Backbone.View.extend({
    //TODO: culling in the object layer
    width: 140, //pixels
    labelHeight: 20, //pixels
    className: "world-object text-center",
    initialize: function () {
        this.img = null;
        this.listenTo(this.model, "change", this.update);
        this.listenTo(this.model, "destroy-view", this.remove);
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
        return el;
    },
    update: function () {
        var model = this.model;
        var xShift = this.width / 2;
        var yShift = model.get("height") / 2;
        var pos = Victor.fromArray(model.get("pos"))
                  .subtract(new Victor(xShift, yShift));
        var angle = -model.get("angle") - 90;
        this.$el.css({left: pos.x, bottom: pos.y});
        this.img.css({"-webkit-transform": "rotate(" + angle + "deg)"});
    }
});


var Layer;
var Background;
var ViewPort;
var UnitView;

Layer = Backbone.View.extend({
    attributes: {
        draggable: false
    },
    className: "layer",
    initialize: function (options) {
        this.paralaxIndex = options.paralaxIndex;
        this.listenTo(this.model, "change:pos", this.updatePos);
    },
    updatePos: function () {
        var el = this.$el;
        var camera = this.model;
        var idx = this.paralaxIndex;
        var d = Victor.fromArray(camera.get("pos"))
                .subtract(Victor.fromArray(camera.previous("pos")))
                .multiply(new Victor(idx, idx));
        el.css({
            left: parseInt(el.css("left"), 10) - d.x,
            bottom: parseInt(el.css("bottom"), 10) - d.y
        });
    }
    //TODO: culling
});


var Background = Layer.extend({
    id: "background",
    tagName: "img",
    events: {
        click: "click",
        mousedown: "mouseDown",
        mousemove: "mouseMove"
    },
    initialize: function (options) {
        Layer.prototype.initialize.call(this, options);
        this.area = options.area;
        this.ui = options.ui;
        this.listenTo(this.model, "change:width change:height", this.render);
        this.listenTo(this.area, "change:background", this.render);
        this.mousePos = null;
    },
    render: function () {
        var camera = this.model;
        var bg = this.area.get("background");
        this.$el.attr("src", "img/" + bg);
        this.$el.width(camera.get("width")).height(camera.get("height"));
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

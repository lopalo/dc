
var Layer;
var Background;
var ViewPort;
var ViewPortBorder;
var UnitView;

Layer = Backbone.View.extend({
    className: "layer",
    initialize: function (options) {
        this.paralaxIndex = options.paralaxIndex;
        this.listenTo(this.model, "change:x change:y", this.updatePos);
    },
    updatePos: function () {
        var el = this.$el;
        var camera = this.model;
        var idx = this.paralaxIndex;
        //TODO: use vectors
        var dx = (camera.get("x") - camera.previous("x")) * idx;
        var dy = (camera.get("y") - camera.previous("y")) * idx;
        el.css({
            left: parseInt(el.css("left"), 10) - dx,
            bottom: parseInt(el.css("bottom"), 10) - dy
        });
    }
    //TODO: culling
});


var Background = Layer.extend({
    id: "background",
    tagName: "img",
    events: {click: "click"},
    initialize: function (options) {
        Layer.prototype.initialize.call(this, options);
        this.area = options.area;
        this.listenTo(this.model, "change:width change:height", this.render);
        this.listenTo(this.area, "change:background", this.render);
    },
    render: function () {
        var camera = this.model;
        var bg = this.area.get("background");
        this.$el.attr("src", "img/" + bg);
        this.$el.width(camera.get("width")).height(camera.get("height"));
    },
    click: function (e) {
        var height = this.model.get("height");
        this.trigger("click", [e.offsetX, height - e.offsetY]);
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


ViewPortBorder = Backbone.View.extend({
    events: {
        mouseenter: "startMoveCamera",
        mouseleave: "stopMoveCamera"
    },
    initialize: function (options) {
        this.direction = options.direction;
        this.periodId = null;
        _.bindAll(this, "updateCamera");
    },
    startMoveCamera: function () {
        this.periodId = setInterval(this.updateCamera, 50);
    },
    stopMoveCamera: function () {
        if (this.periodId !== null) {
            clearInterval(this.periodId);
        }
        this.periodId = null;
    },
    updateCamera: function () {
        this.model.moveTo(this.direction);
    },
    remove: function () {
        Backbone.View.prototype.remove.call(this);
        this.stopMoveCamera();
    }
});


UnitView = Backbone.View.extend({
    //TODO: fix centering of an image
    containerSize: 140, //pixels
    className: "world-object text-center",
    initialize: function () {
        this.img = null;
        this.listenTo(this.model, "change", this.update);
        this.listenTo(this.model, "destroy-view", this.remove);
    },
    render: function () {
        var el = this.$el;
        var model = this.model;
        el.css({width: this.containerSize, height: this.containerSize});
        $("<div></div>").html(model.get("name")).appendTo(el);
        this.img = $("<img>", {src: "img/ship.png"})
            .css({
                width: model.get("width"),
                height: model.get("height")
            })
            .appendTo(el);
        this.update();
        return el;
    },
    update: function () {
        var model = this.model;
        var pos = model.get("pos");
        var angle = -model.get("angle") - 90;
        //TODO: use vectors
        this.$el.css({
            left: pos[0] - this.containerSize / 2,
            bottom: pos[1] - this.containerSize / 2,
        });
        this.img.css({"-webkit-transform": "rotate(" + angle + "deg)"});
    }
});

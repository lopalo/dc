

var Layer;
var Area;
var Camera;
var Background;
var ViewPort;
var WorldObject;
var Unit;
var UnitView;


function World(viewportEl, connection, userName) {
    var camera;
    var area;
    this.connection = connection;
    this.areaId = null;
    this.uid = userName;
    this.area = area = new Area();
    this.camera = camera = new Camera();
    this.viewport = new ViewPort({el: viewportEl, model: camera});
    this.backgroundLayer = new Background({
        paralaxIndex: 0,
        model: camera,
        area: area
    });
    this.objectLayer = new Layer({paralaxIndex: 1, model: camera});
    this.backgroundLayer.$el.appendTo(viewportEl);
    this.objectLayer.$el.appendTo(viewportEl);
    this.objectModels = {};
    this.documentFragment = null;
    _.bindAll(this, "addObject", "removeObject");
}
_.extend(World.prototype, Backbone.Events);
_.extend(World.prototype, {
    listenToConnection: function () {
        this.listenTo(this.connection, "area", this.dispatch);
    },
    dispatch: function (data) {
        var cmd = data.cmd;
        var handler = "handle" + cmd.charAt(0).toUpperCase() + cmd.slice(1);
        this[handler](data.body);
    },
    handleInit: function (data) {
        this.area.set({id: data.areaId, background: data.areaId + ".jpg"});
    },
    objectsInfo: function (objects) {
        var objectModels = this.objectModels;
        var addObject = this.addObject;
        this.createDocumentFragment();
        _.each(objects, function (data) {
            if (_.has(objectModels, data.id)) { return; }
            addObject(data);
        });
        this.applyDocumentFragment(this.objectLayer);
    },
    handleTick: function (data) {
    //TODO: skip it if an area ident is wrong
        if (this.area.get("id") === null) { return; }
        //TODO: check if need to delay
        this.doHandleTick(data);
    },
    doHandleTick: function (data) {
        var removeObject = this.removeObject;
        var objectModels = this.objectModels;
        //TODO: use sets
        var unknownIdents = [];
        var idents = [];
        if (this.area.get("id") === null) { return; }
        //TODO: process delay
        _.each(data.objects, function (value) {
            idents.push(value.id);
            if (!(_.has(objectModels, value.id))) {
                unknownIdents.push(value.id);
            } else {
                objectModels[value.id].set(value);
            }
        });
        if (!(_.isEmpty(unknownIdents))) {
            this.connection.request(
                "area.get_objects_info",
                unknownIdents,
                this.objectsInfo,
                this
            );
        }
        _.each(_.difference(_.keys(objectModels), idents), removeObject);
    },
    createDocumentFragment: function () {
        if (this.documentFragment !== null) { return; }
        this.documentFragment = $(document.createDocumentFragment());
    },
    applyDocumentFragment: function (layer) {
        if (this.documentFragment === null) { return; }
        this.documentFragment.appendTo(layer.$el);
        this.documentFragment = null;
    },
    addObject: function (data) {
        var model;
        var view;
        switch (data.tag) {
            case "User":
                model = new Unit(data);
                view = new UnitView({model: model});
                break
            default:
                throw "Unknown type " + data.tag
        }
        this.objectModels[data.id] = model;
        view.render().appendTo(this.documentFragment);
    },
    removeObject: function (ident) {
        this.objectModels[ident].destroy();
        delete this.objectModels[ident];
    }
});


Layer = Backbone.View.extend({
    className: "layer"
    //TODO: culling
});


var Background = Layer.extend({
    tagName: "img",
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
    }
});

Area = Backbone.Model.extend({
    defaults: {
        id: null,
        background: null
    }
});

Camera = Backbone.Model.extend({
    defaults: {
        x: 0,
        y: 0,
        height: 0,
        width: 0
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


WorldObject = Backbone.Model.extend({
    defaults: {
        pos: [0, 0],
        height: 100,
        width: 50,
        angle: 0
    },
});


Unit = WorldObject.extend({
    defaults: _.extend({
        name: "unit"
    }, WorldObject.prototype.defaults)
});


UnitView = Backbone.View.extend({
    //TODO: fix positioning
    //TODO: fix cropping when overflowed
    className: "world-object text-center",
    initialize: function () {
        this.listenTo(this.model, "change", this.update);
    },
    render: function () {
        var el = this.$el;
        var model = this.model;
        $("<div></div>")
            .html(model.get("name"))
            .addClass("unit-title")
            .appendTo(el);
        $("<img>", {src: "img/ship.png"})
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
        this.$el.css({
            left: pos[0],
            top: pos[1]
        });
    }
});

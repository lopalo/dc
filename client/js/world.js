
function World(viewportEl, connection, userName) {
    //TODO: pass a controler object
    this.viewportEl = viewportEl;
    this.connection = connection;
    this.areaId = null;
    this.uid = userName;
    this.area = new Area();
    this.camera = new Camera();
    this.viewport = new ViewPort({el: viewportEl, model: this.camera});
    this.backgroundLayer = null;
    this.objectLayer = null;
    this.objectModels = {};
    this.viewportBorders = {};
    this.documentFragment = null;
    this.setupLayers();
    this.setupBorders();
}
World.prototype = Object.create(Backbone.Events);
_.extend(World.prototype, {
    constructor: World,
    setupLayers: function () {
        this.backgroundLayer = new Background({
            paralaxIndex: 0,
            model: this.camera,
            area: this.area
        });
        this.objectLayer = new Layer({paralaxIndex: 1, model: this.camera});
        this.backgroundLayer.$el.appendTo(this.viewportEl);
        this.objectLayer.$el.appendTo(this.viewportEl);
        this.listenTo(this.backgroundLayer, "click", this.backgroundClick);
    },
    setupBorders: function () {
        var borders = this.viewportBorders;
        var viewportEl = this.viewportEl;
        var camera = this.camera;
        //TODO: do it in a cycle
        //TODO: add corners
        borders.top = new ViewPortBorder({
            el: viewportEl.find("#viewport-top-border"),
            model: camera,
            direction: [0, 1]
        });
        borders.bottom = new ViewPortBorder({
            el: viewportEl.find("#viewport-bottom-border"),
            model: camera,
            direction: [0, -1]
        });
        borders.left = new ViewPortBorder({
            el: viewportEl.find("#viewport-left-border"),
            model: camera,
            direction: [-1, 0]
        });
        borders.right = new ViewPortBorder({
            el: viewportEl.find("#viewport-right-border"),
            model: camera,
            direction: [1, 0]
        });

    },
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
        this.createDocumentFragment();
        _.each(objects, function (data) {
            if (_.has(objectModels, data.id)) { return; }
            this.addObject(data);
        }, this);
        this.applyDocumentFragment(this.objectLayer);
    },
    handleTick: function (data) {
    //TODO: skip it if an area ident is wrong
        if (this.area.get("id") === null) { return; }
        //TODO: check if need to delay
        this.doHandleTick(data);
    },
    doHandleTick: function (data) {
        var objectModels = this.objectModels;
        //TODO: use sets to improve time complexity
        var idents = [];
        var unknownIdents = [];
        var excessIdents = [];
        if (this.area.get("id") === null) { return; }
        //TODO: process delay
        _.each(data.objects, function (value) {
            idents.push(value.id);
            if (!_.has(objectModels, value.id)) {
                unknownIdents.push(value.id);
            } else {
                objectModels[value.id].set(value);
            }
        });
        if (!_.isEmpty(unknownIdents)) {
            this.connection.request(
                "area.get_objects_info",
                unknownIdents,
                this.objectsInfo,
                this
            );
        }
        excessIdents = _.difference(_.keys(objectModels), idents);
        _.each(excessIdents, this.removeObject, this);
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
        this.objectModels[ident].trigger("destroy-view");
        delete this.objectModels[ident];
    },
    backgroundClick: function (pos) {
        //TODO: a controller object
        pos[0] += this.camera.get("x");
        pos[1] += this.camera.get("y");
        this.connection.send("area.move_to", pos);
    }
});



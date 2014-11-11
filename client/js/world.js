
function World(viewportEl, connection, ui, userName) {
    this.viewportEl = viewportEl;
    this.connection = connection;
    this.ui = ui;
    this.areaId = null;
    this.uid = userName;
    this.serverTimeDiff = 0;
    this.animationId = null;
    this.area = new Area();
    this.camera = new Camera();
    this.viewport = new ViewPort({el: viewportEl, model: this.camera});
    this.backgroundLayer = null;
    this.objectLayer = null;
    this.objectModels = {};
    this.viewportBorders = {};
    this.documentFragment = null;
    _.bindAll(this, "animationStep");
    this.setupLayers();
    this.setupBorders();
    this.listenToConnection();
    this.listenToUI();
    this.requestAnimationFrame();
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
        //TODO: move it to the UI component
        //TODO: do it in a cycle
        //TODO: add corners to add more directions for moving;
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
        //TODO: use $.camelCase
        //TODO: use command names with a hyphen
        var handler = "handle" + cmd.charAt(0).toUpperCase() + cmd.slice(1);
        this[handler](data.body);
    },
    handleInit: function (data) {
        this.serverTimeDiff = this.getTime() - data.timestamp;
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
        var objectModels = this.objectModels;
        //TODO: use sets to improve time complexity
        var idents = [];
        var unknownIdents = [];
        var excessIdents = [];
        if (this.area.get("id") === null) { return; }
        _.each(data.objects, function (value) {
            idents.push(value.id);
            if (!_.has(objectModels, value.id)) {
                unknownIdents.push(value.id);
            } else {
                objectModels[value.id].set(value);
            }
        }, this);
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
        this.animationStep();
    },
    getTime: function () {
        return performance.now();
    },
    getServerTime: function () {
        return this.getTime() - this.serverTimeDiff;
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
                break;
            default:
                throw "Unknown type " + data.tag;
        }
        this.objectModels[data.id] = model;
        view.render().appendTo(this.documentFragment);
    },
    removeObject: function (ident) {
        this.objectModels[ident].trigger("destroy-view");
        delete this.objectModels[ident];
    },
    backgroundClick: function (pos) {
        //TODO: use vectors
        pos[0] += this.camera.get("x");
        pos[1] += this.camera.get("y");
        this.connection.send("area.move_to", pos);
    },
    listenToUI: function () {
        this.listenTo(this.ui, "focus-to-myself", this.showMyself);
    },
    showMyself: function () {
        //TODO
    },
    requestAnimationFrame: function () {
        this.animationId = requestAnimationFrame(this.animationStep);
    },
    cancelAnimationFrame: function () {
        cancelAnimationFrame(this.animationId);
    },
    animationStep: function () {
        //TODO: fix performance degradation (maybe change CSS in a native DOM)
        //TODO: decrease frame rate
        var timestamp = this.getServerTime();
        this.requestAnimationFrame(this.animationStep);
        _.each(_.values(this.objectModels), function (model) {
            if (model.applyActions !== undefined) {
                model.applyActions(timestamp);
            }
        }, this);
    }
});



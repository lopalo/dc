var FPS = 30;

function World(viewportEl, connection, ui, userName) {
    this.viewportEl = viewportEl;
    this.connection = connection;
    this.ui = ui;
    this.uid = userName;
    this.serverTimeDiff = 0;
    this.animationLoopId = null;
    this.animationId = null;
    this.area = new Area();
    this.camera = new Camera();
    this.viewport = new ViewPort({el: viewportEl, model: this.camera});
    this.backgroundLayer = null;
    this.objectLayer = null;
    this.objectModels = {};
    this.viewportBorders = {};
    this.documentFragment = null;
    _.bindAll(this, "animationLoopStep", "animationCallback");
    this.setupLayers();
    this.setupBorders();
    this.listenToConnection();
    this.listenToUI();
    this.requestAnimation();
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
            direction: new Victor(0, 1)
        });
        borders.bottom = new ViewPortBorder({
            el: viewportEl.find("#viewport-bottom-border"),
            model: camera,
            direction: new Victor(0, -1)
        });
        borders.left = new ViewPortBorder({
            el: viewportEl.find("#viewport-left-border"),
            model: camera,
            direction: new Victor(-1, 0)
        });
        borders.right = new ViewPortBorder({
            el: viewportEl.find("#viewport-right-border"),
            model: camera,
            direction: new Victor(1, 0)
        });

    },
    listenToConnection: function () {
        this.listenTo(this.connection, "area", this.dispatch);
    },
    dispatch: function (data) {
        var handler = $.camelCase("handle-" + data.cmd);
        this[handler](data.body);
    },

    handleInit: function (data) {
        this.setServerTime(data.timestamp);
        this.area.set({id: data.areaId, background: data.areaId + ".jpg"});
    },
    objectsInfo: function (objects) {
        var objectModels = this.objectModels;
        this.createDocumentFragment();
        _.each(objects, function (data) {
            if (_.has(objectModels, data.id)) return;
            this.addObject(data);
        }, this);
        this.applyDocumentFragment(this.objectLayer);
    },
    handleTick: function (data) {
        if (this.area.get("id") !== data.areaId) return;
        if (data.timestamp > this.getServerTime()) {
            this.setServerTime(data.timestamp);
            console.log("Server time is updated");
        }
        var objectModels = this.objectModels;
        //TODO: use sets to improve time complexity
        var idents = [];
        var unknownIdents = [];
        var excessIdents = [];
        if (this.area.get("id") === null) return;
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
                "area.get-objects-info",
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
    setServerTime: function (serverTimestamp) {
        this.serverTimeDiff = this.getTime() - serverTimestamp;
    },
    getServerTime: function () {
        return this.getTime() - this.serverTimeDiff;
    },
    createDocumentFragment: function () {
        if (this.documentFragment !== null) return;
        this.documentFragment = $(document.createDocumentFragment());
    },
    applyDocumentFragment: function (layer) {
        if (this.documentFragment === null) return;
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
        pos.add(Victor.fromObject(this.camera.attributes));
        this.connection.send("area.move-to", pos.toArray());
    },
    listenToUI: function () {
        this.listenTo(this.ui, "focus-to-myself", this.showMyself);
    },
    showMyself: function () {
        //TODO
    },
    requestAnimation: function () {
        this.animationLoopId = setTimeout(this.animationLoopStep, 1000 / FPS);
    },
    animationLoopStep: function () {
        this.animationId = requestAnimationFrame(this.animationCallback);
    },
    animationCallback: function () {
        this.requestAnimation();
        this.animationStep();
    },
    stopAnimationLoop: function () {
        clearTimeout(this.animationLoopId);
        cancelAnimationFrame(this.animationId);
    },
    animationStep: function () {
        var timestamp = this.getServerTime();
        _.each(_.values(this.objectModels), function (model) {
            if (model.applyActions !== undefined) {
                model.applyActions(timestamp);
            }
        }, this);
    }
});



var FPS = 30;

function World(viewportEl, connection, user, area, ui) {
    this.viewportEl = viewportEl;
    this.connection = connection;
    this.user = user;
    this.area = area;
    this.ui = ui;
    this.serverTimeDiff = 0;
    this.animationLoopId = null;
    this.animationId = null;
    this.camera = new Camera();
    this.viewport = new ViewPort({el: viewportEl, model: this.camera});
    this.backgroundLayer = null;
    this.objectLayer = null;
    this.objectModels = {};
    this.appearanceReasons = {}; //FIXME: remove old events
    this.disappearanceReasons = {};
    this.firstEnter = true;
    this.documentFragment = null;
    _.bindAll(this, "animationLoopStep", "animationCallback");
    this.setupLayers();
    this.listenToConnection();
    this.listenToUI();
    this.requestAnimation();
}
World.prototype = Object.create(Backbone.Events);
_.extend(World.prototype, {
    constructor: World,
    setupLayers: function () {
        this.backgroundLayer = new Background({
            parallaxIndex: 0,
            model: this.camera,
            area: this.area,
            ui: this.ui
        });
        this.midgroundLayer = new Midground({
            parallaxIndex: .3,
            model: this.camera,
        });
        this.objectLayer = new Layer({parallaxIndex: 1, model: this.camera});
        this.backgroundLayer.$el.appendTo(this.viewportEl);
        this.midgroundLayer.$el.appendTo(this.viewportEl);
        this.objectLayer.$el.appendTo(this.viewportEl);
        this.listenTo(this.backgroundLayer, "move-to", this.moveTo);
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
        _.each(_.keys(this.objectModels), this.removeObject, this);
        this.area.set({areaId: data.areaId, background: data.areaId + ".jpg"});
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
        if (this.area.get("areaId") !== data.areaId) return;
        if (data.timestamp > this.getServerTime()) {
            this.setServerTime(data.timestamp);
            console.log("Server time updated");
        }
        var objectModels = this.objectModels;
        //TODO: use sets to improve time complexity
        var idents = [];
        var unknownIdents = [];
        var excessIdents = [];
        _.each(data.events, function (ev) {
            if (ev.tag === "Appearance") {
                this.appearanceReasons[ev.ident] = ev.aReason;
            }
            if (ev.tag === "Disappearance") {
                this.disappearanceReasons[ev.ident] = ev.dReason;
            }
        }, this);
        _.each(data.objects, function (value) {
            if (_.has(this.disappearanceReasons, value.id)) return;
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
        this.disappearanceReasons = [];
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
        var reason = this.appearanceReasons[data.id];
        var model;
        var view;
        delete this.appearanceReasons[data.id];
        switch (data.tag) {
            case "User":
                if (!reason && data.id === this.user.get("userId")) {
                    reason = this.firstEnter ? "LogIn" : "Entry";
                }
                model = new Unit(data);
                view = new UserView({model: model, reason: reason});
                break;
            default:
                throw "Unknown type " + data.tag;
        }
        this.objectModels[data.id] = model;
        view.render().appendTo(this.documentFragment);
    },
    removeObject: function (ident) {
        var reason = this.disappearanceReasons[ident];
        this.objectModels[ident].trigger("destroy-view", reason);
        delete this.objectModels[ident];
    },
    moveTo: function (pos) {
        this.connection.send("area.move-to", pos.toArray());
    },
    listenToUI: function () {
        this.listenTo(this.ui, "focus-to-myself", this.showMyself);
        this.listenTo(this.ui, "ignite", this.ignite);
        this.listenTo(this.ui, "enter-area", this.enterArea);
    },
    showMyself: function () {
        var self = this.objectModels[this.user.get("userId")];
        if (self === undefined) return;
        this.camera.focusTo(Victor.fromArray(self.get("pos")));
    },
    ignite: function () {
        this.connection.send("area.ignite", 10);
    },
    enterArea: function (areaId) {
        this.firstEnter = false;
        var self = this.objectModels[this.user.get("userId")];
        self.trigger("destroy-view", "Exit");
        this.connection.send("area.enter-area", areaId);
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



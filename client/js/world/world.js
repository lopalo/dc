
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var models = require("./models");
    var views = require("./views");


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
        this.camera = new models.Camera();
        this.viewport = new views.ViewPort({
            el: viewportEl,
            model: this.camera
        });
        this.backgroundLayer = null;
        this.objectLayer = null;
        this.objectModels = {};
        this.appearanceReasons = {}; //FIXME: remove old events
        this.disappearanceReasons = {};
        this.firstEnter = true;
        this.eventHandler = new EventHandler(this);
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
            this.backgroundLayer = new views.Background({
                parallaxIndex: 0,
                model: this.camera,
                area: this.area,
                ui: this.ui
            });
            this.midgroundLayer = new views.Midground({
                parallaxIndex: 0.3,
                model: this.camera,
            });
            this.objectLayer = new views.Layer({
                parallaxIndex: 1,
                model: this.camera
            });
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
            this.area.set({
                areaId: data.areaId,
                background: data.areaId + ".jpg"
            });
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
                    this.appearanceReasons[ev.userId] = ev.aReason;
                }
                if (ev.tag === "Disappearance") {
                    this.disappearanceReasons[ev.objId] = ev.dReason;
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
            this.animationStep();
            this.eventHandler.process(data.events);
            excessIdents = _.difference(_.keys(objectModels), idents);
            _.each(excessIdents, this.removeObject, this);
            this.disappearanceReasons = [];
        },
        getTime: function () {
            return window.performance.now();
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
            var isSelf = false;
            var model;
            var view;
            delete this.appearanceReasons[data.id];
            switch (data.tag) {
                case "User":
                    isSelf = data.id === this.user.get("userId");
                    if (!reason && isSelf) {
                        reason = this.firstEnter ? "LogIn" : "Entry";
                    }
                    model = new models.User(data);
                    view = new views.UserView({
                        model: model,
                        reason: reason,
                        isSelf: isSelf,
                        ui: this.ui
                    });
                    this.listenTo(view, "shot", this.shot);
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
        shot: function (ident) {
            this.connection.send("area.shoot", ident);
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
            this.animationLoopId = setTimeout(
                this.animationLoopStep,
                1000 / FPS
            );
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


    function EventHandler(world) {
        this.world = world;
    }
    EventHandler.prototype = {
        process: function (events) {
            var world = this.world;
            world.createDocumentFragment();
            _.each(events, function (ev) {
                var handler = this["handle" + ev.tag];
                if (!handler) return;
                handler.call(this, ev);
            }, this);
            world.applyDocumentFragment(world.objectLayer);

        },
        handleShot: function (ev) {
            var world = this.world;
            var objects = world.objectModels;
            var start = Victor.fromArray(objects[ev.shooter].get("pos"));
            var end = Victor.fromArray(objects[ev.target].get("pos"));
            var delta = end.subtract(start);
            var endEffect = function () { shot.remove(); };
            var shot = $("<div></div>")
                .addClass("world-shot world-slow-effect")
                .css({
                    "-webkit-transform": "rotate(" + -delta.angle() + "rad)",
                    width: delta.length(),
                    left: start.x,
                    bottom: start.y,
                })
                .appendTo(world.documentFragment)
               .bind("webkitTransitionEnd", endEffect);
            _.delay(endEffect, 3100); // must be synchronized with the transition duration
            _.delay(function () { shot.css("opacity", 0); }, 100);
        }
    };
    return World;
});

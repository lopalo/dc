
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var TweenLite = require("tween-lite");
    var pixi = require("pixi");

    var settings = require("json!settings.json").stage;
    var models = require("models");
    var stageModels = require("./models");
    var stageViews = require("./views");


    function StageController(viewportEl, controller) {
        this._viewportEl = viewportEl;
        this._controller = controller;
        this._area = controller.getAreaModel();

        this._serverTimeDiff = 0;
        this._appearanceReasons = {}; //FIXME: remove old signals
        this._disappearanceReasons = {};
        this._firstEnter = true;

        this._animationLoopId = null;
        this._interpolationLoopId = null;

        this._camera = new stageModels.Camera();
        this._objectModels = new models.ModelStore();

        this._renderer = null;
        this._stage = null;
        this._backgroundLayer = null;
        this._midgroundLayers = [];
        this._objectLayer = null;
        this._objectViews = {};

        this._signalHandler = new SignalHandler(this);

        _.bindAll(this, "_animationCallback",
                  "_interpolationStep", "_resizeWindow");
    }
    StageController.prototype = Object.create(Backbone.Events);
    _.extend(StageController.prototype, {
        constructor: StageController,
        init: function () {
            var renderer;
            this._renderer = renderer = new pixi.autoDetectRenderer();
            this._stage = new pixi.Container();

            renderer.view.id = "renderer";
            renderer.autoResize = true;
            this._viewportEl.append(renderer.view);
            $(window).on("resize", this._resizeWindow);

            this._setupLayers();
            this._controller.listenToStageBackground(this._backgroundLayer);
            this._resizeWindow();
        },
        start: function () {
            this._requestAnimation();
            this._startInterpolation();
        },
        destroy: function () {
            this.stopListening();
            this._stopLoops();
            this._renderer.destroy(true);
            this._viewportEl.empty();
            $(window).off("resize", this._resizeWindow);
        },
        dispatch: function (data) {
            var handler = $.camelCase("_handle-" + data.cmd);
            this[handler](data.body);
        },
        showMyself: function () {
            var self = this._getSelf();
            if (self === undefined) return;
            this._camera.focusTo(Victor.fromArray(self.get("pos")));
        },
        moveCamera: function (delta) {
            this._camera.move(delta);
        },
        enterArea: function () {
            this._firstEnter = false;
            this._objectViews[this._controller.getUserId()].destroy("Exit");
        },
        getObjectModel: function (id) {
            return this._objectModels.roModels[id];
        },
        getCameraPos: function () {
            return Victor.fromArray(this._camera.get("pos"));
        },
        _resizeWindow: function () {
            var width = $(window).width();
            var height = $(window).height();
            this._renderer.resize(width, height);
            this._camera.set({width: width, height: height});
        },
        _setupLayers: function () {
            var stage = this._stage;
            var camera = new models.ReadOnlyProxy(this._camera);
            this._backgroundLayer = new stageViews.Background({
                camera: camera,
                area: this._area
            });
            this._backgroundLayer.createContainer(stage);
            this._objectLayer = new stageViews.ObjectLayer({
                camera: camera,
                parallaxIndex: 1,
            });
            this._objectLayer.createContainer(stage);
            //TODO: create midgrounds
        },
        _handleInit: function (data) {
            this._setServerTime(data.timestamp);
            _.each(_.keys(this._objectModels.models),
                   this._removeObject, this);
        },
        _objectsInfo: function (objects) {
            var objectModels = this._objectModels.models;
            _.each(objects, function (data) {
                if (_.has(objectModels, data.id)) return;
                this._addObject(data);
            }, this);
        },
        _handleTick: function (data) {
            if (this._area.get("areaId") !== data.areaId) return;
            if (data.timestamp > this._getServerTime()) {
                this._setServerTime(data.timestamp);
                console.log("Server time updated");
            }
            var objectModels = this._objectModels.models;
            //TODO: use sets to improve time complexity
            var idents = [];
            var unknownIdents = [];
            var excessIdents = [];
            _.each(data.signals, function (signal) {
                if (signal.tag === "Appearance") {
                    this._appearanceReasons[signal.userId] = signal.aReason;
                }
                if (signal.tag === "Disappearance") {
                    this._disappearanceReasons[signal.objId] = signal.dReason;
                }
            }, this);
            _.each(data.objects, function (value) {
                if (_.has(this._disappearanceReasons, value.id)) return;
                idents.push(value.id);
                if (!_.has(objectModels, value.id)) {
                    unknownIdents.push(value.id);
                } else {
                    objectModels[value.id].set(value);
                }
            }, this);
            if (!_.isEmpty(unknownIdents)) {
                this._controller.request(
                    "area.get-objects-info",
                    unknownIdents,
                    this._objectsInfo,
                    this
                );
            }
            this._interpolationStep();
            this._signalHandler.process(data.signals);
            excessIdents = _.difference(_.keys(objectModels), idents);
            _.each(excessIdents, this._removeObject, this);
            this._disappearanceReasons = [];
        },
        _getTime: function () {
            return window.performance.now();
        },
        _setServerTime: function (serverTimestamp) {
            this._serverTimeDiff = this._getTime() - serverTimestamp;
        },
        _getServerTime: function () {
            return this._getTime() - this._serverTimeDiff;
        },
        _addObject: function (data) {
            var objectModels = this._objectModels;
            var reason = this._appearanceReasons[data.id];
            var isSelf = false;
            var model;
            var roModel;
            var view;
            delete this._appearanceReasons[data.id];

            switch (data.tag) {
                case "User":
                    model = new stageModels.User(data);
                    break;
                default:
                    throw "No model for type " + data.tag;
            }
            objectModels.set(data.id, model);
            roModel = objectModels.roModels[data.id];

            switch (data.tag) {
                case "User":
                    isSelf = this._controller.isSelf(data.id);
                    if (!reason && isSelf) {
                        reason = this._firstEnter ? "LogIn" : "Entry";
                    }
                    view = new stageViews.User({
                        model: roModel,
                        reason: reason,
                        isSelf: isSelf,
                    });
                    break;
                default:
                    throw "No view for type " + data.tag;
            }
            this._objectViews[data.id] = view;
            this._controller.listenToStageObjectView(view);
            this._objectLayer.addView(view);
            if (isSelf) {
                this.showMyself();
            }
        },
        _removeObject: function (ident) {
            var reason = this._disappearanceReasons[ident];
            var view = this._objectViews[ident];
            this.stopListening(view);
            this._controller.stopListening(view);
            view.destroy(reason);
            this._objectModels.cleanup(ident);
            delete this._objectViews[ident];
        },
        _getSelf: function () {
            return this._objectModels.models[this._controller.getUserId()];
        },

        _requestAnimation: function () {
            this._animationLoopId = window.requestAnimationFrame(
                this._animationCallback
            );
        },
        _animationCallback: function () {
            this._requestAnimation();
            this._backgroundLayer.resize();
            this._renderer.render(this._stage);
        },

        _startInterpolation: function () {
            this._interpolationLoopId = window.setInterval(
                this._interpolationStep,
                settings["interpolation-msec"]
            );
        },
        _interpolationStep: function () {
            if (!settings["enable-interpolation"]) return;
            var timestamp = this._getServerTime();
            _.each(_.values(this._objectModels.models), function (model) {
                if (model.applyActions !== undefined) {
                    model.applyActions(timestamp);
                }
            }, this);
        },
        _stopLoops: function () {
            window.cancelAnimationFrame(this._animationLoopId);
            window.clearInterval(this._interpolationLoopId);
        },
    });


    function SignalHandler(stageController) {
        this._stageController = stageController;
    }
    SignalHandler.prototype = {
        constructor: SignalHandler,
        process: function (signals) {
            _.each(signals, function (signal) {
                var handler = this["handle" + signal.tag];
                if (!handler) return;
                handler.call(this, signal);
            }, this);
            //TODO: use a public method of StageController to add sprites to
            //      the objectLayer
        },
        handleShot: function (signal) {
            var getObj = this._stageController.getObjectModel;
            var start = Victor.fromArray(getObj(signal.shooter).get("pos"));
            var end = Victor.fromArray(getObj(signal.target).get("pos"));
            var delta = end.subtract(start);
            var onComplete = function () { shot.remove(); };
            //TODO: do not use tween-lite-css
            return;
            var shot = $("<div></div>")
                .addClass("world-shot")
                .css({
                    transform: "rotate(" + -delta.angle() + "rad)",
                    width: delta.length(),
                    left: start.x,
                    bottom: start.y,
                })
            TweenLite.to(shot, 2, {opacity: 0, onComplete: onComplete});
        }
    };
    return StageController;
});


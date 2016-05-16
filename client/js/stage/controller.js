
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");
    var pixi = require("pixi");

    var settings = require("json!settings.json").stage;
    var models = require("models");
    var stageModels = require("./models");
    var stageViews = require("./views");
    var SignalHandler = require("./signal-handler");

    var user = require("./objects/user");
    var gate = require("./objects/gate");
    var asteroid = require("./objects/asteroid");
    var controlPoint = require("./objects/control-point");


    function StageController(viewportEl, controller) {
        this._viewportEl = viewportEl;
        this._controller = controller;
        this._area = controller.getAreaModel();

        this._serverTimeDiffList = [];
        this._serverTimeDiff = 0;
        this._tickQueue = [];
        this._appearanceReasons = {};
        this._disappearanceReasons = {};
        this._firstEnter = true;

        this._animationLoopId = null;
        this._interpolationLoopId = null;
        this._timeCorrectionLoopId = null;
        this._tickCheckLoopId = null;

        this._camera = new stageModels.Camera();
        this._objectModels = new models.ModelStore();

        this._renderer = null;
        this._stage = null;
        this._backgroundLayer = null;
        this._midgroundLayers = [];
        this._objectLayer = null;
        this._objectViews = {};

        _.bindAll(this,
                  "_animationCallback",
                  "_interpolationStep",
                  "_serverTimeDiffCorrection",
                  "_tickCheck",
                  "_resizeWindow",
                  "_addEffect",
                  "getObjectModel");

        this._signalHandler = new SignalHandler({
            addEffect: this._addEffect,
            getObjectModel: this.getObjectModel
        });
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
            this._startTimeCorrection();
            this._startTickCheck();
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
            this._camera.focusTo(Victor.fromArray(self.get("pos")).invertY());
        },
        moveCamera: function (delta) {
            this._camera.move(delta);
        },
        enterArea: function () {
            this._appearanceReasons = {};
            this._firstEnter = false;
            this._removeObject(this._controller.getUserId(), "Exit");
        },
        getObjectModel: function (id) {
            return this._objectModels.roModels[id];
        },
        getVisibleUserIds: function () {
            var models = _.values(this._objectModels.models);
            models = _.filter(models, function (model) {
                return model instanceof user.User;
            });
            return _.map(models, function (model) { return model.get("id"); });
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
            this._midgroundLayers = [
                new stageViews.Midground({camera: camera,
                                          parallaxIndex: 0.3}),
                new stageViews.Midground({camera: camera,
                                          parallaxIndex: 0.7}),
            ];
            _.each(this._midgroundLayers, function (layer) {
                layer.createContainer(stage);
            }, this);
            this._objectLayer = new stageViews.ObjectLayer({
                camera: camera,
                parallaxIndex: 1,
            });
            this._objectLayer.createContainer(stage);
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
        _handleTick: function (parts) {
            var push = Array.prototype.push;
            var areaId = this._area.get("areaId");
            var data = {objects: [], signals: []};
            _.each(parts, function (part) {
                if (areaId !== part.areaId) return;
                data.areaId = part.areaId;
                data.timestamp = part.timestamp;
                push.apply(data.objects, part.objects || []);
                push.apply(data.signals, part.signals || []);
            });

            this._saveServerTime(data.timestamp);
            this._tickQueue.push(data);
            this._tickCheck();
        },
        _tick: function (data) {
            var objectModels = this._objectModels.models;
            var idents = [];
            var unknownIdents = [];
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
            _.chain(objectModels)
                .keys().difference(idents)
                .each(this._removeObject, this);
            this._disappearanceReasons = {};
        },
        _getTime: function () {
            return window.performance.now();
        },
        _setServerTime: function (serverTimestamp) {
            this._serverTimeDiff = this._getTime() - serverTimestamp;
            this._serverTimeDiffList = [];
        },
        _saveServerTime: function (serverTimestamp) {
            this._serverTimeDiffList.push(this._getTime() - serverTimestamp);
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
                    model = new user.User(data);
                    break;
                case "Gate":
                    model = new gate.Gate(data);
                    break;
                case "Asteroid":
                    model = new asteroid.Asteroid(data);
                    break;
                case "CP":
                    model = new controlPoint.ControlPoint(data);
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
                    view = new user.UserView({
                        model: roModel,
                        reason: reason,
                        isSelf: isSelf,
                    });
                    break;
                case "Gate":
                    view = new gate.GateView({model: roModel});
                    break;
                case "Asteroid":
                    view = new asteroid.AsteroidView({model: roModel});
                    break;
                case "CP":
                    view = new controlPoint.ControlPointView({model: roModel});
                    break;
                default:
                    throw "No view for type " + data.tag;
            }
            this._objectViews[data.id] = view;
            this._controller.listenToStageObjectView(view);
            this._objectLayer.addView(view);
            if (isSelf) {
                this.showMyself();
                this._controller.setSelfStageObject(data.id);
            }
        },
        _removeObject: function (ident, reason) {
            var view = this._objectViews[ident];
            reason = reason || this._disappearanceReasons[ident];
            this._signalHandler.deleteStageObject(ident);
            this._controller.deleteStageObject(ident);
            this.stopListening(view);
            this._controller.stopListening(view);
            view.destroy(reason);
            this._objectModels.cleanup(ident);
            delete this._objectViews[ident];
        },
        _addEffect: function (container, tween) {
            this._objectLayer.addEffect(container, tween);
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
            _.each(_.values(this._objectViews), function (view) {
                view.update();
            });
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
        _startTimeCorrection: function () {
            this._timeCorrectionLoopId = window.setInterval(
                this._serverTimeDiffCorrection,
                1000
            );
        },
        _serverTimeDiffCorrection: function () {
            var diffList = this._serverTimeDiffList;
            if (_.isEmpty(diffList)) return;
            var sum = _.reduce(diffList, function (a, b) { return a + b; });
            var correction = sum / _.size(diffList) - this._serverTimeDiff;
            var sign = Math.sign(correction);
            var abs = Math.abs(correction);
            abs = _.min([abs, settings["time-correction-msec-per-sec"]]);
            this._serverTimeDiff += abs * sign;
            this._serverTimeDiffList = [];
        },
        _startTickCheck: function () {
            this._tickCheckLoopId = window.setInterval(
                this._tickCheck,
                settings["tick-check-period-msec"]
            );
        },
        _tickCheck: function () {
            var queue = this._tickQueue;
            var ts = this._getServerTime();
            while (!_.isEmpty(queue) && queue[0].timestamp <= ts) {
                this._tick(queue.shift());
                ts = this._getServerTime();
            }

        },
        _stopLoops: function () {
            window.cancelAnimationFrame(this._animationLoopId);
            window.clearInterval(this._interpolationLoopId);
            window.clearInterval(this._timeCorrectionLoopId);
            window.clearInterval(this._tickCheckLoopId);
        },
    });

    return StageController;
});


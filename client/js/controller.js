
define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");

    var models = require("models");
    var UI = require("ui");
    var StageController = require("stage/controller");
    var stageModels = require("stage/models");


    function Controller(gameEl, connection) {
        this._gameEl = gameEl;
        this._connection = connection;
        this._models = new models.ModelStore();
        this._models.set("user", new models.User());
        this._models.set("area", new models.Area());
        this._models.set("ui", new UI.UI());
        this._stageController = new StageController(gameEl.find("#viewport"),
                                                    this);
        _.bindAll(this, "destroy");

        this._cursorPos = null;
        this._cursorPath = [];
        this._selectedStageObjectId = null;

    }
    Controller.prototype = Object.create(Backbone.Events);
    _.extend(Controller.prototype, {
        constructor: Controller,
        init: function () {
            var uiViews;
            var stage = this._stageController;
            var conn = this._connection;
            var user = this._models.models.user;
            var roModels = this._models.roModels;
            conn.once("user.init", user.set, user);
            uiViews = UI.setupUI(this._gameEl.find("#ui"),
                                 roModels.ui,
                                 roModels.user,
                                 roModels.area);
            this.listenTo(conn, "area.init", this._initArea);
            this._listenToUI(uiViews);
            stage.init();
            stage.listenTo(conn, "area", stage.dispatch);
            this._gameEl.show();
        },
        start: function () {
            this._stageController.start();
        },
        destroy: function () {
            this._gameEl.hide();
            this.stopListening();
            this._models.cleanupAll();
            this._stageController.destroy();
        },
        listenToStageBackground: function (bg) {
            this.listenTo(bg, "click", this._backgroundClick);
            this.listenTo(bg, "mouseDown", this._backgroundMouseDown);
            this.listenTo(bg, "mouseUp", this._backgroundMouseUp);
            this.listenTo(bg, "mouseMove", this._backgroundMouseMove);
        },
        listenToStageObjectView: function (view) {
            this.listenTo(view, "click", this._stageObjectViewClick);
            this.listenTo(view, "mouseUp", this._stageObjectViewMouseUp);
            this.listenTo(view, "mouseOver", this._stageObjectViewMouseOver);
            this.listenTo(view, "mouseOut", this._stageObjectViewMouseOut);
        },
        request: function () {
            this._connection.request.apply(this._connection, arguments);
        },
        getAreaModel: function () {
            return this._models.roModels.area;
        },
        getUserId: function () {
            return this._models.roModels.user.get("userId");
        },
        isSelf: function (id) {
            return this.getUserId() === id;
        },
        _listenToUI: function (uiViews) {
            this.listenTo(uiViews.focusButton, "click", this._showMyself);
            this.listenTo(uiViews.recoverButton, "click", this._recover);
            this.listenTo(uiViews.areaSelector, "select", this._enterArea);
            this.listenTo(uiViews.controlModeSelector, "select",
                                        this._changeControlMode);
        },
        _showMyself: function () {
            this._stageController.showMyself();
        },
        _recover: function () {
            this._connection.send("area.recover", 1);
        },
        _enterArea: function (areaId) {
            this._stageController.enterArea();
            this._connection.send("area.enter-area", areaId);
        },
        _changeControlMode: function (mode) {
            this._models.models.ui.set("controlMode", mode);
        },
        _initArea: function (data) {
            this._models.models.area.set({
                areaId: data.areaId,
                background: data.areaId.replace("area:", "") + ".jpg" //FIXME
            });
        },
        _stageObjectViewClick: function (ident) {
            var ui = this._models.models.ui;
            var model = this._stageController.getObjectModel(ident);
            var isUser = model.isInstanceOf(stageModels.User);
            switch (ui.get("controlMode")) {
                case "shot":
                    if (isUser && !this.isSelf(ident)) {
                        this._connection.send("area.shoot", ident);
                    }
                    break;
                case "view":
                    this._selectStageObject(ident);
                    break;
            }
        },
        _stageObjectViewMouseUp: function (ident) {
        },
        _stageObjectViewMouseOver: function (ident) {
            this._cursorPos = null;
        },
        _stageObjectViewMouseOut: function () {
        },
        _backgroundClick: function () {
            this._unselectStageObject();

        },
        _backgroundMouseDown: function () {
            this._cursorPos = null;
            this._cursorPath = [];
        },
        _backgroundMouseMove: function (pos) {
            pos = new Victor(pos.x, pos.y);
            switch (this._models.models.ui.get("controlMode")) {
                case "view":
                    if (this._cursorPos === null) {
                        this._cursorPos = pos;
                    }
                    var previousPos = this._cursorPos.clone();
                    this._cursorPos = pos;
                    this._stageController.moveCamera(previousPos.subtract(pos));
                    break;
                case "move":
                    this._cursorPath.push(pos);
                    break;
            }

        },
        _backgroundMouseUp: function () {
            if (this._models.models.ui.get("controlMode") !== "move") return;
            var cameraPos = this._stageController.getCameraPos();
            var positions = _.map(this._cursorPath, function (pos) {
                return pos.add(cameraPos).invertY().toArray();
            });
            if (!_.isEmpty(positions)) {
                this._connection.send("area.move-along-route", positions);
            }
            this._cursorPath = [];
        },
        _selectStageObject: function (ident) {
            var model = this._stageController.getObjectModel(ident);
            var ui = this._models.models.ui;
            this._unselectStageObject();
            ui.set("selectedObjectType", model.objectType);
            ui.set("selectedObjectInfo", model.getInfoForUI());
            this.listenTo(model, "change", this._selectedStageObjectChanged);
            this._selectedStageObjectId = ident;
        },
        _selectedStageObjectChanged: function (model) {
            var ui = this._models.models.ui;
            ui.set("selectedObjectInfo", model.getInfoForUI());
        },
        _unselectStageObject: function () {
            var ident = this._selectedStageObjectId;
            var model = this._stageController.getObjectModel(ident);
            var ui = this._models.models.ui;
            ui.set("selectedObjectType", "nothing");
            ui.set("selectedObjectInfo", {});
            this.stopListening(model, "change");
            this._selectedStageObjectId = null;
        }
    });
    return Controller;
});


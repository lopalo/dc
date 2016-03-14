
define(function (require) {
    var _ = require("underscore");
    var Backbone = require("backbone");
    var Victor = require("victor");

    var models = require("models");
    var setupUI = require("ui/ui");
    var StageController = require("stage/controller");
    var WindowsController = require("windows-controller");
    var ObjectContextController = require("object-context-controller");


    function Controller(gameEl, connection) {
        this._gameEl = gameEl;
        this._connection = connection;
        this._models = new models.ModelStore();
        this._models.set("user", new models.User());
        this._models.set("area", new models.Area());
        this._models.set("messages", new models.Messages());
        this._models.set("ui", new models.UI());
        this._stageController = new StageController(gameEl.find("#viewport"),
                                                    this);
        var options = {
            models: this._models,
            connection: this._connection,
            stageController: this._stageController
        };
        this._windowsController = new WindowsController(options);
        this._objectContextController = new ObjectContextController(options);

        _.bindAll(this, "destroy");

        this._cursorPos = null;
        this._cursorPath = [];

    }
    Controller.prototype = Object.create(Backbone.Events);
    _.extend(Controller.prototype, {
        constructor: Controller,
        init: function () {
            var uiViews;
            var stage = this._stageController;
            var conn = this._connection;
            var uiModels = _.pick(
                this._models.roModels,
                ["ui", "user", "area", "messages"]
            );
            uiViews = setupUI(this._gameEl.find("#ui"), uiModels);
            this.listenTo(conn, "area.init", this._initArea);
            this.listenTo(conn, "user", this._userDispatch);
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
            this._windowsController.destroy();
            this._objectContextController.destroy();
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
        setSelfStageObject: function (ident) {
            var model = this._stageController.getObjectModel(ident);
            var ui = this._models.models.ui;
            this._deleteSelfStageObject();
            ui.set("selfObjectInfo", model.getInfoForUI());
            this.listenTo(model, "change", this._selfStageObjectChanged);
        },
        deleteStageObject: function (ident) {
            if (this.isSelf(ident)) {
                this._deleteSelfStageObject();
            }
            if (ident === this._models.models.ui.get("selectedObjectId")) {
                this._unselectStageObject();
            }
            this._windowsController.deleteStageObject(ident);
            this._objectContextController.deleteStageObject(ident);
        },
        _initArea: function (data) {
            this._models.models.area.set({
                areaId: data.areaId,
                background: data.areaId.replace("area:", "") + ".jpg" //FIXME
            });
        },
        _userDispatch: function (data) {
            var handler = $.camelCase("_handle-" + data.cmd);
            this[handler](data.body);
        },
        _handleInit: function (data) {
            this._models.models.user.set(data);
        },
        _handleAddMessages: function (messages) {
            this._models.models.messages.add(messages);
        },
        _listenToUI: function (uiViews) {
            var common = uiViews.common;

            this.listenTo(common.focusButton, "click", this._showMyself);
            this.listenTo(common.recoverButton, "click", this._recover);
            this.listenTo(common.cancelPullButton, "click", this._cancelPull);

            this.listenTo(uiViews.controlMode.controlModeSelector,
                          "select", this._changeControlMode);

            this._windowsController.listenToUI(uiViews.windows);
            this._objectContextController.listenToUI(uiViews.objectContext);

        },
        _showMyself: function () {
            this._stageController.showMyself();
        },
        _recover: function () {
            this._connection.send("area.recover", 1);
        },
        _cancelPull: function () {
            this._connection.send("area.cancel-pull", null);
        },
        _changeControlMode: function (mode) {
            this._models.models.ui.set("controlMode", mode);
        },
        _stageObjectViewClick: function (ident) {
            if (this._windowIsActive()) return;
            var ui = this._models.models.ui;
            switch (ui.get("controlMode")) {
                case "shot":
                    break;
                case "view":
                    this._selectStageObject(ident);
                    break;
            }
        },
        _stageObjectViewMouseUp: function (ident) {
            if (this._windowIsActive()) return;
        },
        _stageObjectViewMouseOver: function (ident) {
            if (this._windowIsActive()) return;
            this._cursorPos = null;
        },
        _stageObjectViewMouseOut: function () {
            if (this._windowIsActive()) return;
        },
        _backgroundClick: function (pos) {
            if (this._windowIsActive()) {
                this._models.models.ui.set("activeWindow", null);
                return;
            }
            this._unselectStageObject();
            var cameraPos = this._stageController.getCameraPos();
            pos.add(cameraPos).invertY();
            switch (this._models.models.ui.get("controlMode")) {
                case "shot":
                    this._connection.send("area.shoot", pos.toArray());
                    break;
            }

        },
        _backgroundMouseDown: function () {
            if (this._windowIsActive()) return;
            this._cursorPos = null;
            this._cursorPath = [];
        },
        _backgroundMouseMove: function (pos) {
            if (this._windowIsActive()) return;
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
            if (this._windowIsActive()) return;
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
            ui.set("selectedObjectId", ident);
            ui.set("selectedObjectType", model.objectType);
            ui.set("selectedObjectInfo", model.getInfoForUI());
            this.listenTo(model, "change", this._selectedStageObjectChanged);
        },
        _selectedStageObjectChanged: function (model) {
            var ui = this._models.models.ui;
            ui.set("selectedObjectInfo", model.getInfoForUI());
        },
        _unselectStageObject: function () {
            var ui = this._models.models.ui;
            var ident = ui.get("selectedObjectId");
            var model = this._stageController.getObjectModel(ident);
            if (model === undefined) return;
            ui.set("selectedObjectId", null);
            ui.set("selectedObjectType", "nothing");
            ui.set("selectedObjectInfo", {});
            this.stopListening(model, "change");
        },
        _selfStageObjectChanged: function (model) {
            var ui = this._models.models.ui;
            ui.set("selfObjectInfo", model.getInfoForUI());
        },
        _deleteSelfStageObject: function () {
            var ident = this.getUserId();
            var model = this._stageController.getObjectModel(ident);
            var ui = this._models.models.ui;
            if (model === undefined) return;
            ui.set("selfObjectInfo", {});
            this.stopListening(model, "change");
        },
        _windowIsActive: function () {
            return this._models.models.ui.get("activeWindow") !== null;
        }
    });
    return Controller;
});


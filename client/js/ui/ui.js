
define(function (require) {
    var common = require("./common");
    var windowsCommon = require("./windows/common");
    var controlMode = require("./control-mode");
    var objectContext = require("./object-context");
    var messages = require("./windows/messages");
    var WorldmapWindow = require("./windows/worldmap");



    function setupUI(uiEl, models) {
        uiEl.find(".selectpicker").selectpicker();
        return {
            common: setupCommon(uiEl.find("#ui-common"), models),
            objectContext: setupObjectContext(uiEl.find("#ui-object-context"),
                                              models),
            controlMode: setupControlMode(uiEl.find("#ui-control-mode"),
                                          models),
            windows: setupWindows(uiEl.find("#ui-windows"), models)
        };
    }


    function setupCommon(commonEl, models) {
        var focusButton = new common.Button({
            el: commonEl.find("#ui-focus-to-myself"),
            model: models.ui,
        });
        var recoverButton = new common.Button({
            el: commonEl.find("#ui-recover"),
            model: models.ui,
        });
        var cancelPullButton = new common.Button({
            el: commonEl.find("#ui-cancel-pull"),
            model: models.ui,
        });

        return {
            focusButton: focusButton,
            recoverButton: recoverButton,
            cancelPullButton: cancelPullButton,
        };
    }


    function setupObjectContext(objectContextEl, models) {
        var areaSelector = new objectContext.SelectArea({
            el: objectContextEl.find("#ui-select-area"),
            model: models.ui,
            area: models.area,
            worldmap: models.worldmap
        });
        var captureButton = new objectContext.CaptureButton({
            el: objectContextEl.find("#ui-capture"),
            model: models.ui,
        });
        var pullButton = new objectContext.PullButton({
            el: objectContextEl.find("#ui-pull"),
            model: models.ui,
        });
        var objectInfo = new objectContext.ObjectInfo({
            el: objectContextEl.find("#ui-object-info"),
            model: models.ui,
        });
        return {
            areaSelector: areaSelector,
            captureButton: captureButton,
            pullButton: pullButton,
            objectInfo: objectInfo,
        };
    }


    function setupControlMode(controlModeEl, models) {
        var controlModeSelector = new controlMode.SelectControlMode({
            el: controlModeEl.find("#ui-select-control-mode"),
            model: models.ui
        });
        return {
            controlModeSelector: controlModeSelector,
        };
    }


    function setupWindows(windowsEl, models) {
        new windowsCommon.Header({
            el: windowsEl.find("#ui-window-header"),
            model: models.ui,
        });
        var closeButton = new windowsCommon.CloseButton({
            el: windowsEl.find("#ui-close-window"),
            model: models.ui,
        });
        var messagesButton = new messages.MessagesButton({
            el: windowsEl.find("#ui-open-messages"),
            model: models.ui,
            messages: models.messages
        });
        var messagesWindow = new messages.MessagesWindow({
            el: windowsEl.find("#ui-messages-window"),
            model: models.ui,
            messages: models.messages
        });
        var worldmapButton = new windowsCommon.WindowButton({
            el: windowsEl.find("#ui-open-worldmap"),
            model: models.ui,
            windowName: "worldmap"
        });
        var worldmapWindow = new WorldmapWindow({
            el: windowsEl.find("#ui-worldmap-window"),
            model: models.ui,
            area: models.area,
            worldmap: models.worldmap
        });
        return {
            closeButton: closeButton,
            messagesButton: messagesButton,
            messagesWindow: messagesWindow,
            worldmapButton: worldmapButton,
            worldmapWindow: worldmapWindow
        };
    }

    return setupUI;
});

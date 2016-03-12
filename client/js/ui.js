
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    require("bootstrap");
    require("bootstrap-select");


    var UI;
    var UIView;
    var Button;
    var SelectControlMode;
    var SelectArea;
    var CaptureButton;
    var PullButton;
    var ObjectInfo;
    var WindowButton;
    var CloseButton;
    var MessagesWindow;

    function setupUI(uiEl, models) {
        var focusButton = new Button({
            el: uiEl.find("#ui-focus-to-myself"),
            model: models.ui,
        });
        var recoverButton = new Button({
            el: uiEl.find("#ui-recover"),
            model: models.ui,
        });
        var controlModeSelector = new SelectControlMode({
            el: uiEl.find("#ui-select-control-mode"),
            model: models.ui
        });
        var areaSelector = new SelectArea({
            el: uiEl.find("#ui-select-area"),
            model: models.ui,
            area: models.area,
            user: models.user
        });
        var captureButton = new CaptureButton({
            el: uiEl.find("#ui-capture"),
            model: models.ui,
        });
        var pullButton = new PullButton({
            el: uiEl.find("#ui-pull"),
            model: models.ui,
        });
        var cancelPullButton = new Button({
            el: uiEl.find("#ui-cancel-pull"),
            model: models.ui,
        });
        var objectInfo = new ObjectInfo({
            el: uiEl.find("#ui-object-info"),
            model: models.ui,
        });

        var closeButton = new CloseButton({
            el: uiEl.find("#ui-close-window"),
            model: models.ui,
        });
        var messagesButton = new WindowButton({
            el: uiEl.find("#ui-open-messages"),
            model: models.ui,
            windowName: "messages"
        });
        var messagesWindow = new MessagesWindow({
            el: uiEl.find("#ui-messages-window"),
            model: models.ui,
            messages: models.messages
        });

        uiEl.find(".selectpicker").selectpicker();
        return {
            focusButton: focusButton,
            recoverButton: recoverButton,
            controlModeSelector: controlModeSelector,
            areaSelector: areaSelector,
            captureButton: captureButton,
            pullButton: pullButton,
            cancelPullButton: cancelPullButton,
            objectInfo: objectInfo,
            closeButton: closeButton,
            messagesButton: messagesButton,
            messagesWindow: messagesWindow
        };
    }


    UI = Backbone.Model.extend({
        controlModes: ["view", "move", "shot"],
        windows: [null, "messages"],
        defaults: function () {
            return {
                controlModes: this.controlModes,
                controlMode: this.controlModes[1],
                windows: this.windows,
                activeWindow: this.windows[0],
                selectedObjectType: "nothing",
                selectedObjectInfo: {},
                selfObjectInfo: {}
            };
        }
    });


    UIView = Backbone.View.extend({
        initialize: function () {
            this.listenTo(this.model, "cleanup", this.destroy);
        },
        destroy: function () {
            this.stopListening();
            this.undelegateEvents();
        }
    });


    Button = UIView.extend({
        events: {
            click: "click",
        },
        click: function () {
            this.trigger("click");
        },
    });


    SelectControlMode = UIView.extend({
        keyMap: [49, 50, 51],
        initialize: function () {
            SelectControlMode.__super__.initialize.call(this);
            this.listenTo(this.model, "change:controlMode", this.render);
            _.bindAll(this, "keyDown", "buttonClick", "rotate");
            $(window).on("keydown", this.keyDown);
            $(window).on("wheel", this.rotate);
            _.each(this.$el.find("button"), function (btn) {
                $(btn).on("click", _.partial(this.buttonClick, btn));
            }, this);
            this.render();
        },
        render: function () {
            _.each(this.$el.find("button"), function (btn) {
                var $btn = $(btn);
                $btn.removeClass("active");
                if ($btn.val() === this.model.get("controlMode")) {
                    $btn.addClass("active");
                }
            }, this);
        },
        buttonClick: function (btn) {
            this.trigger("select", $(btn).val());
        },
        keyDown: function (e) {
            var idx = this.keyMap.indexOf(e.keyCode);
            if (idx === -1) return;
            var mode = this.model.get("controlModes")[idx];
            if (mode === undefined) return;
            this.trigger("select", mode);

        },
        rotate: function (e) {
            var modes = this.model.get("controlModes");
            var mode = this.model.get("controlMode");
            var sign = Math.sign(e.originalEvent.wheelDelta);
            var newMode = modes[modes.indexOf(mode) - sign];
            if (newMode !== undefined) {
                this.trigger("select", newMode);
            }
        },
        destroy: function () {
            SelectControlMode.__super__.destroy.call(this);
            $(window).off("keydown", this.keyDown);
            $(window).off("wheel", this.rotate);
            _.each(this.$el.find("button"), function (btn) {
                $(btn).off("click");
            }, this);
        }
    });


    SelectArea = UIView.extend({
        events: {
            change: "change",
        },
        template: _.template("<option value='<%= v %>'><%= n %></option>"),
        initialize: function (options) {
            SelectArea.__super__.initialize.call(this);
            this.area = options.area;
            this.user = options.user;
            this.listenTo(this.area, "change:areaId", this.render);
            this.listenTo(this.user, "change:areas", this.render);
            this.listenTo(this.model, "change:selectedObjectType", this.render);
            this.render();
        },
        render: function () {
            var el = this.$el;
            var gateSelected = this.model.get("selectedObjectType") === "gate";
            this.$el.parent().toggle(gateSelected);
            if (!gateSelected) return;
            el.empty();
            _.each(this.user.get("areas"), function (aid) {
                el.append(this.template({
                    v: aid,
                    n: aid.replace("area:", "")
                }));
            }, this);
            this.$el.val(this.area.get("areaId"));
            this.$el.selectpicker("refresh");
        },
        change: function () {
            this.trigger("select", this.$el.val());
        },
    });


    CaptureButton = Button.extend({
        initialize: function () {
            CaptureButton.__super__.initialize.call(this);
            this.listenTo(this.model, "change:selectedObjectType", this.render);
            this.listenTo(this.model, "change:selectedObjectInfo", this.render);
            this.render();
        },
        render: function () {
            var type = this.model.get("selectedObjectType");
            var info = this.model.get("selectedObjectInfo");
            this.$el.toggle(type === "control-point" && info.owner === null);
        }
    });


    PullButton = Button.extend({
        initialize: function () {
            PullButton.__super__.initialize.call(this);
            this.listenTo(this.model, "change:selectedObjectType", this.render);
            this.listenTo(this.model, "change:selectedObjectInfo", this.render);
            this.render();
        },
        render: function () {
            var type = this.model.get("selectedObjectType");
            var info = this.model.get("selectedObjectInfo");
            this.$el.toggle(type === "asteroid" && info.pullAllowed);
        }
    });


    ObjectInfo = UIView.extend({
        initialize: function () {
            ObjectInfo.__super__.initialize.call(this);
            this.listenTo(this.model, "change:selectedObjectInfo", this.render);
        },
        render: function () {
            var el = this.$el;
            var type = this.model.get("selectedObjectType");
            var info = this.model.get("selectedObjectInfo");
            if (!_.isEmpty(info)) {
                var tmpl = $("#" + type + "-info-template");
                if (tmpl) {
                    el.html(_.template(tmpl.html())(info));
                    el.show();
                }
            } else {
                el.hide();
            }
        }
    });


    WindowButton = UIView.extend({
        events: {
            click: "click",
        },
        initialize: function (options) {
            WindowButton.__super__.initialize.call(this);
            this.windowName = options.windowName;
            this.listenTo(this.model, "change:activeWindow", this.render);
            this.render();
        },
        render: function () {
            this.$el.toggle(this.model.get("activeWindow") === null);
        },
        click: function () {
            this.trigger("activateWindow", this.windowName);
        },
    });


    CloseButton = WindowButton.extend({
        initialize: function () {
            CloseButton.__super__.initialize.call(this, {windowName: null});
        },
        render: function () {
            this.$el.toggle(this.model.get("activeWindow") !== null);
        },
    });


    MessagesWindow = UIView.extend({
        initialize: function (options) {
            MessagesWindow.__super__.initialize.call(this);
            this.messages = options.messages;
            this.messagesEl = this.$el.find("#ui-messages");
            this.inputEl = this.$el.find("#ui-messages-input");
            this.sendBtn = this.$el.find("#ui-send-message");
            _.bindAll(this, "send", "keyDown");
            $(window).on("keydown", this.keyDown);
            this.sendBtn.on("click", this.send);
            this.listenTo(this.messages, "update", this.addMessages);
            this.listenTo(this.model, "change:activeWindow", this.render);
            this._addMessages(this.messages.toArray());
            this.render();
        },
        destroy: function () {
            MessagesWindow.__super__.destroy.call(this);
            this.messagesEl.empty();
            $(window).off("keydown", this.keyDown);
            this.inputEl.off();
            this.sendBtn.off();
            this.messagesEl = null;
            this.inputEl = null;
            this.sendBtn = null;
        },
        render: function () {
            this.$el.toggle(this.model.get("activeWindow") === "messages");
            this._scrollToLast();
        },
        keyDown: function (e) {
            if (e.keyCode !== 13) return;
            if (this.model.get("activeWindow") !== "messages") return;
            this.send();
        },
        send: function () {
            var msg = $.trim(this.inputEl.val());
            if (!msg) return;
            this.inputEl.val("");
            this.trigger("send", msg);
        },
        addMessages: function (messages, options) {
            this._addMessages(options.changes.added);
        },
        _addMessages: function (messages) {
            var messagesFragment = $(document.createDocumentFragment());
            _.each(messages, function (message) {
                this._renderMessage(message).appendTo(messagesFragment);
            }, this);
            messagesFragment.appendTo(this.messagesEl);
            this._scrollToLast();
        },
        _renderMessage: function (m) {
            var msg = "<li>";
            msg += m.get("name") + ": ";
            msg += m.get("text");
            msg += "</li>";
            return $(msg);
        },
        _scrollToLast: function () {
            this.messagesEl.scrollTop(this.messagesEl.prop("scrollHeight"));
        }
    });


    return {
        UI: UI,
        setupUI: setupUI
    };
});

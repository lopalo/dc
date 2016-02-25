
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

    function setupUI(uiEl, ui, user, area) {
        var focusButton = new Button({
            el: uiEl.find("#ui-focus-to-myself"),
            model: ui,
        });
        var recoverButton = new Button({
            el: uiEl.find("#ui-recover"),
            model: ui,
        });
        var controlModeSelector = new SelectControlMode({
            el: uiEl.find("#control-mode"),
            model: ui
        });
        var areaSelector = new SelectArea({
            el: uiEl.find("#select-area"),
            model: ui,
            area: area,
            user: user
        });
        var captureButton = new CaptureButton({
            el: uiEl.find("#ui-capture"),
            model: ui,
        });
        var pullButton = new PullButton({
            el: uiEl.find("#ui-pull"),
            model: ui,
        });
        var cancelPullButton = new Button({
            el: uiEl.find("#ui-cancel-pull"),
            model: ui,
        });
        var objectInfo = new ObjectInfo({
            el: uiEl.find("#ui-object-info"),
            model: ui,
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
            objectInfo: objectInfo
        };
    }


    UI = Backbone.Model.extend({
        controlModes: ["view", "move", "shot"],
        defaults: function () {
            return {
                controlModes: this.controlModes,
                controlMode: this.controlModes[1],
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
        events: {
            change: "change",
        },
        initialize: function () {
            SelectControlMode.__super__.initialize.call(this);
            this.listenTo(this.model, "change:controlMode", this.render);
            _.bindAll(this, "keyDown");
            $(window).on("keydown", this.keyDown);
            this.render();
        },
        render: function () {
            this.$el.val(this.model.get("controlMode"));
            this.$el.selectpicker("render");
        },
        change: function () {
            this.trigger("select", this.$el.val());
        },
        keyDown: function (e) {
            var idx = this.keyMap.indexOf(e.keyCode);
            if (idx === -1) return;
            var mode = this.model.get("controlModes")[idx];
            if (mode === undefined) return;
            this.trigger("select", mode);

        },
        destroy: function () {
            SelectControlMode.__super__.destroy.call(this);
            $(window).off("keydown", this.keyDown);
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
            if (this.model.get("selectedObjectType") === "gate") {
                el.parent().show();
            } else {
                el.parent().hide();
                return;
            }
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
            var el = this.$el;
            var type = this.model.get("selectedObjectType");
            var info = this.model.get("selectedObjectInfo");
            if (type === "control-point" && info.owner === null) {
                el.show();
            } else {
                el.hide();
            }
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
            var el = this.$el;
            var type = this.model.get("selectedObjectType");
            var info = this.model.get("selectedObjectInfo");
            if (type === "asteroid" && info.pullAllowed) {
                el.show();
            } else {
                el.hide();
            }
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

    return {
        UI: UI,
        setupUI: setupUI
    };
});

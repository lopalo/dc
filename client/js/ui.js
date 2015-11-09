
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

    function setupUI(uiEl, ui, user, area) {
        var focusButton = new Button({
            el: uiEl.find("#ui-focus-to-myself"),
            model: ui,
        });
        var igniteButton = new Button({
            el: uiEl.find("#ui-ignite"),
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
        uiEl.find(".selectpicker").selectpicker();
        return {
            focusButton: focusButton,
            igniteButton: igniteButton,
            controlModeSelector: controlModeSelector,
            areaSelector: areaSelector,
        };
    }


    UI = Backbone.Model.extend({
        controlModes: ["view", "move", "shot"],
        defaults: function () {
            return {
                controlModes: this.controlModes,
                controlMode: this.controlModes[1]
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


    SelectArea = Backbone.View.extend({
        events: {
            change: "change",
        },
        template: _.template("<option value='<%= v %>'><%= v %></option>"),
        initialize: function (options) {
            SelectArea.__super__.initialize.call(this);
            this.area = options.area;
            this.user = options.user;
            this.listenTo(this.area, "change:areaId", this.render);
            this.listenTo(this.user, "change:areas", this.render);
            this.render();
        },
        render: function () {
            var el = this.$el;
            el.empty();
            _.each(this.user.get("areas"), function (areaId) {
                el.append(this.template({v: areaId}));
            }, this);
            this.$el.val(this.area.get("areaId"));
            this.$el.selectpicker("refresh");
        },
        change: function () {
            this.trigger("select", this.$el.val());
        },
    });

    return {
        UI: UI,
        setupUI: setupUI
    };
});


define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    require("bootstrap");
    require("bootstrap-select");


    var UI;
    var Button;
    var SelectControlMode;
    var SelectArea;

    function setupUI(uiEl, ui, user, area) {
        uiEl.find(".selectpicker").selectpicker();
        new Button({
            el: uiEl.find("#ui-focus-to-myself"),
            model: ui,
            event: "focus-to-myself"
        });
        new Button({
            el: uiEl.find("#ui-ignite"),
            model: ui,
            event: "ignite"
        });
        new SelectControlMode({el: uiEl.find("#control-mode"), model: ui});
        new SelectArea({
            el: uiEl.find("#select-area"),
            model: ui,
            area: area,
            user: user
        });
    }

    UI = Backbone.Model.extend({
        controlModes: ["view", "move", "shot"],
        defaults: function () {
            return {
                controlMode: this.controlModes[1]
            };
        }
    });

    Button = Backbone.View.extend({
        events: {
            click: "click",
        },
        initialize: function (options) {
            this.event = options.event;
        },
        click: function () {
            this.model.trigger(this.event);
        },
    });

    SelectControlMode = Backbone.View.extend({
        keyMap: [49, 50, 51],
        events: {
            change: "changeMode",
        },
        initialize: function () {
            this.listenTo(this.model, "change:controlMode", this.render);
            _.bindAll(this, "keyDown");
            $(document).on("keydown", this.keyDown);
            this.render();
        },
        render: function () {
            this.$el.val(this.model.get("controlMode"));
            this.$el.selectpicker("render");
        },
        changeMode: function () {
            this.model.set("controlMode", this.$el.val());
        },
        keyDown: function (e) {
            var idx = this.keyMap.indexOf(e.keyCode);
            if (idx === -1) return;
            var mode = this.model.controlModes[idx];
            if (mode === undefined) return;
            this.model.set("controlMode", mode);

        },
        remove: function () {
            Backbone.View.prototype.remove.call(this);
            $(document).off("keydown", this.keyDown);
        }

    });


    SelectArea = Backbone.View.extend({
        events: {
            change: "changeArea",
        },
        template: _.template("<option value='<%= v %>'><%= v %></option>"),
        initialize: function (options) {
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
        changeArea: function () {
            this.model.trigger("enter-area", this.$el.val());
        },
    });
    return {
        UI: UI,
        setupUI: setupUI
    };
});

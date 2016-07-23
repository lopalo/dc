
define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    require("bootstrap");
    require("bootstrap-select");

    var common = require("ui/common");


    var SelectControlMode;


    SelectControlMode = common.UIView.extend({
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

    return {
        SelectControlMode: SelectControlMode
    };
});

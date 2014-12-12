var UI;
var FocusMyselfButton;
var SelectControlMode;

function setupUI(uiEl) {
    var ui = new UI();
    new FocusMyselfButton({el: uiEl.find("#focus-to-myself"), model: ui});
    new SelectControlMode({el: uiEl.find("#control-mode"), model: ui});
    return ui;
}

UI = Backbone.Model.extend({
    controlModes: ["view", "move"],
    defaults: function () {
        return {
            controlMode: this.controlModes[0]
        };
    }
});

FocusMyselfButton = Backbone.View.extend({
    events: {
        click: "click",
    },
    click: function () {
        this.model.trigger("focus-to-myself");
    },
});

SelectControlMode = Backbone.View.extend({
    keyMap: [49, 50],
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

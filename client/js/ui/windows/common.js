define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    require("bootstrap");
    require("bootstrap-select");

    var common = require("ui/common");


    var WindowButton;
    var CloseButton;
    var Header;

    WindowButton = common.UIView.extend({
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
        render: function () {}
    });


    Header = common.UIView.extend({
        text: {
            messages: "Messages",
            worldmap: "World Map",
        },
        initialize: function () {
            Header.__super__.initialize.call(this);
            this.listenTo(this.model, "change:activeWindow", this.render);
            this.render();
        },
        render: function () {
            var el = this.$el;
            var activeWindow = this.model.get("activeWindow");
            el.parent().parent().toggle(activeWindow !== null);
            el.text(this.text[activeWindow]);
        },
    });


    return {
        WindowButton: WindowButton,
        CloseButton: CloseButton,
        Header: Header
    };
});



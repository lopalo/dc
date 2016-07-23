define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    require("bootstrap");
    require("bootstrap-select");

    var common = require("ui/common");
    var windowsCommon = require("./common");

    var MessagesWindow;
    var MessagesButton;


    MessagesWindow = common.UIView.extend({
        messageTemplate: _.template("<li><%= name %>: <%= text %></li>"),
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
            return $(this.messageTemplate(m.attributes));
        },
        _scrollToLast: function () {
            this.messagesEl.scrollTop(this.messagesEl.prop("scrollHeight"));
        }
    });


    MessagesButton = windowsCommon.WindowButton.extend({
        initialize: function (options) {
            this.msgCountEl = this.$el.find("#ui-message-count");
            this.count = 0;
            this.listenTo(options.messages, "update", this.increment);
            MessagesButton.__super__.initialize.call(
                this,
                {windowName: "messages"
            });
        },
        render: function () {
            MessagesButton.__super__.render.call(this);
            this.msgCountEl.text(this.count ? this.count : "");
        },
        destroy: function () {
            MessagesButton.__super__.destroy.call(this);
            this.msgCountEl = null;
        },
        click: function () {
            this.count = 0;
            MessagesButton.__super__.click.call(this);
        },
        increment: function (messages, options) {
            if (this.model.get("activeWindow") === "messages") return;
            if (options.changes.added.length > 1) return; // it's initialization
            this.count += 1;
            this.render();
        },
    });

    return {
        MessagesWindow: MessagesWindow,
        MessagesButton: MessagesButton
    };
});


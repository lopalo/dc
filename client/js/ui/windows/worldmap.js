define(function (require) {
    var $ = require("jquery");
    var _ = require("underscore");
    var Backbone = require("backbone");
    require("bootstrap");
    require("bootstrap-select");

    var common = require("ui/common");

    var WorldmapWindow = common.UIView.extend({
        headerTemplate: _.template(
            '<li class="list-group-item list-group-item-info">' +
                'World Owner: <%= owner === null ? "-" : owner %>' +
            '</li>'
        ),
        areaTemplate: _.template(
            '<li class="list-group-item">' +
                '<%= area %>: <%= owner === null ? "-" : owner %>' +
            '</li>'
        ),
        initialize: function (options) {
            WorldmapWindow.__super__.initialize.call(this);
            this.worldmap = options.worldmap;
            this.listenTo(this.model, "change:activeWindow", this.render);
            this.listenTo(this.worldmap, "change", this.render);
            this.render();
        },
        render: function () {
            var isActive = this.model.get("activeWindow") === "worldmap";
            var el = this.$el;
            var worldmap = this.worldmap;
            var worldOwner = null
            var owners = this.worldmap.values();
            var half = Math.floor(owners.length / 2);
            el.toggle(isActive);
            if (!isActive) return;
            el.empty();
            _.chain(owners)
             .filter()
             .countBy()
             .pairs()
             .each(function (ownerCount) {
                if (ownerCount[1] > half) {
                    worldOwner = ownerCount[0];
                }
            });
            el.append(this.headerTemplate({owner: worldOwner}));
            _.chain(worldmap.keys()).sortBy().each(function (aid) {
                el.append(this.areaTemplate({
                    area: aid.replace("area:", ""),
                    owner: worldmap.get(aid)
                }));
            }, this);
        },
    });

    return WorldmapWindow;
});



define(function (require) {
    var Registry = require("registry");
    var NodeStatistic = require("node-statistic");

    var pages = [
        {
            route: "registry",
            title: "Registry",
            component: Registry
        },
        {
            route: "node-statistic",
            title: "Node Statistic",
            component: NodeStatistic
        },
    ];
    return pages;
});


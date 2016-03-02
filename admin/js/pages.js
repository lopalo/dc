
define(function (require) {
    var Registry = require("registry");
    var NodeStatus = require("node-status");

    var pages = [
        {
            route: "registry",
            title: "Registry",
            component: Registry
        },
        {
            route: "node-status",
            title: "Node Status",
            component: NodeStatus
        },
    ];
    return pages;
});


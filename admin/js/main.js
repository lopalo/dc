
require.config({
    baseUrl: "/ui/js",
    paths: {
        jquery: "vendors/jquery.min",
        bootstrap: "vendors/bootstrap.min",
        mithril: "vendors/mithril.min",
    },
    shim: {
        jquery: {
            exports: "$"
        },
        bootstrap: ["jquery"],
    }
});

require(["app"], function (app) {
    app();
});


require.config({
    baseUrl: "js",
    paths: {
        mithril: "vendors/mithril.min",
    },
});

require(["app"], function (app) {
    app();
});

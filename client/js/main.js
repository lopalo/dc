
require.config({
    baseUrl: "js",
    paths: {
        jquery: "vendors/jquery.min",
        underscore: "vendors/underscore-min",
        backbone: "vendors/backbone-min",
        victor: "vendors/victor.min",
        "query-parser": "vendors/jquery-queryParser.min",
        bootstrap: "vendors/bootstrap.min",
        "bootstrap-select": "vendors/bootstrap-select.min",
        "tween-lite": "vendors/TweenLite.min",
        "tween-lite-css": "vendors/CSSPlugin.min"
    },
    shim: {
        jquery: {
            exports: "$"
        },
        underscore: {
            exports: "_"
        },
        backbone: {
            deps: ["jquery", "underscore"],
            exports: "Backbone"
        },
        victor: {
            exports: "Victor"
        },
        "query-parser": ["jquery"],
        bootstrap: ["jquery"],
        "bootstrap-select": ["bootstrap"],
        "tween-lite": {
            exports: "TweenLite"
        },
        "tween-lite-css": ["tween-lite"],
    }
});

require(["app"], function (app) {
    app();
});

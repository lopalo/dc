
require.config({
    baseUrl: "js",
    paths: {
        jquery: "libs/jquery.min",
        underscore: "libs/underscore-min",
        backbone: "libs/backbone-min",
        victor: "libs/victor.min",
        "query-parser": "libs/jquery-queryParser.min",
        bootstrap: "libs/bootstrap.min",
        "bootstrap-select": "libs/bootstrap-select.min"
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
        "bootstrap-select": ["bootstrap"]
    }
});

require(["app"], function (app) {
    app();
});

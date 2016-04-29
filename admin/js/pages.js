
define(function (require) {
    var Registry = require("registry");
    var NodeStatus = require("node-status");
    var ClusterMesh = require("cluster-mesh");

    var AreaStatus = require("area-status");

    var pages = [
        {
            section: "cluster",
            title: "Cluster",
            pages: [
                {
                    id: "registry",
                    title: "Registry",
                    component: Registry
                },
                {
                    id: "node-status",
                    title: "Node Status",
                    component: NodeStatus
                },
                {
                    id: "mesh",
                    title: "Cluster Mesh",
                    component: ClusterMesh
                }
            ]
        },
        {
            section: "area",
            title: "Area",
            pages: [
                {
                    id: "status",
                    title: "Area Status",
                    component: AreaStatus
                },
            ]
        }
    ];
    return pages;
});

